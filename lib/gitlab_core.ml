let user_agent = "ocaml-gitlab"

module Make
    (Env : Gitlab_s.Env)
    (Time : Gitlab_s.Time)
    (CL : Cohttp_lwt.S.Client) =
struct
  let log_active = ref Env.debug

  let log fmt =
    Printf.ksprintf
      (fun s ->
        match !log_active with
        | false -> ()
        | true -> prerr_endline (">>> GitLab: " ^ s))
      fmt

  type rate = Core

  let string_of_message message =
    message.Gitlab_t.message_message
    ^ Gitlab_t.(
        List.fold_left
          (fun s { error_resource; error_field; error_code; error_message } ->
            let error_field =
              match error_field with None -> "\"\"" | Some x -> x
            in
            let error_message =
              match error_message with None -> "\"\"" | Some x -> x
            in
            Printf.sprintf
              "%s\n> Resource type: %s\n  Field: %s\n  Code: %s\n  Message: %s"
              s error_resource error_field error_code error_message)
          "" message.Gitlab_t.message_errors)

  exception Message of Cohttp.Code.status_code * Gitlab_t.message

  module Response = struct
    type redirect = Temporary of Uri.t | Permanent of Uri.t

    type 'a t = < value : 'a ; redirects : redirect list >

    let value r = r#value

    let redirects r = r#redirects

    let rec final_resource = function
      | [] -> None
      | Permanent uri :: rest -> perm_resource uri rest
      | Temporary uri :: rest -> temp_resource uri rest

    and perm_resource uri = function
      | [] -> Some (Permanent uri)
      | Permanent uri :: rest -> perm_resource uri rest
      | Temporary uri :: rest -> temp_resource uri rest

    and temp_resource uri = function
      | [] -> Some (Temporary uri)
      | (Temporary uri | Permanent uri) :: rest -> temp_resource uri rest

    let wrap : ?redirects:redirect list -> 'a -> 'a t =
     fun ?(redirects = []) v ->
      object
        method value = v

        method redirects = redirects
      end
  end

  module C = Cohttp
  module CLB = Cohttp_lwt.Body

  module Monad = struct
    open Printf
    open Lwt

    (* Each API call results in either a valid response or
     * an HTTP error. Depending on the error status code, it may
     * be retried within the monad, or a permanent failure returned
     *)
    type error =
      | Generic of (C.Response.t * string)
      | Semantic of C.Code.status_code * Gitlab_t.message
      | Bad_response of
          exn * [ `None | `Json of Yojson.Basic.t | `Raw of string ]

    type request = {
      meth : C.Code.meth;
      uri : Uri.t;
      headers : C.Header.t;
      body : string;
    }

    type state = { user_agent : string option; token : string option }

    type 'a signal =
      | Request of request * (request -> 'a signal Lwt.t)
      | Response of 'a
      | Err of error

    type 'a t = state -> (state * 'a signal) Lwt.t

    let string_of_message = string_of_message

    let error_to_string = function
      | Generic (res, body) ->
          Lwt.return
            (sprintf "HTTP Error %s\nHeaders:\n%s\nBody:\n%s\n"
               (C.Code.string_of_status (C.Response.status res))
               (String.concat "" (C.Header.to_lines (C.Response.headers res)))
               body)
      | Semantic (_, message) ->
          Lwt.return ("GitLab API error: " ^ string_of_message message)
      | Bad_response (exn, j) ->
          Lwt.return
            (sprintf "Bad response: %s\n%s" (Printexc.to_string exn)
               (match j with
               | `None -> "<none>"
               | `Raw r -> sprintf "Raw body:\n%s" r
               | `Json j ->
                   sprintf "JSON body:\n%s" (Yojson.Basic.pretty_to_string j)))

    let error err = Err err

    let response r = Response r

    let request ?token:_ ?(params = []) ({ uri; _ } as req) reqfn =
      let uri = Uri.add_query_params' uri params in
      Request ({ req with uri }, reqfn)

    let prepare_headers state headers =
      (* Add User-Agent *)
      let headers =
        C.Header.prepend_user_agent headers
          (user_agent ^ " " ^ C.Header.user_agent)
      in
      let headers =
        match state.user_agent with
        | None -> headers
        | Some ua -> C.Header.prepend_user_agent headers ua
      in
      (* Add access token *)
      match state.token with
      | None -> headers
      | Some token -> C.Header.add headers "Authorization" ("token " ^ token)

    let prepare_request state req =
      { req with headers = prepare_headers state req.headers }

    let rec bind fn x state =
      x state >>= function
      | state, Request (req, reqfn) ->
          reqfn (prepare_request state req) >>= fun r ->
          bind fn (fun state -> Lwt.return (state, r)) state
      | state, Response r -> fn r state
      | state, (Err _ as x) -> Lwt.return (state, x)

    let return r state = Lwt.return (state, Response r)

    let map f m = bind (fun x -> return (f x)) m

    let initial_state = { user_agent = None; token = None }

    let run th =
      bind return th initial_state >>= function
      | _, Request (_, _) ->
          Lwt.fail (Failure "Impossible: can't run unapplied request")
      | _, Response r -> Lwt.return r
      | _, Err (Semantic (status, msg)) -> Lwt.(fail (Message (status, msg)))
      | _, Err e -> Lwt.(error_to_string e >>= fun err -> fail (Failure err))

    let both p1 p2 = bind (fun x -> bind (fun y -> return (x, y)) p2) p1

    let ( >>= ) m f = bind f m

    let ( let* ) m f = bind f m

    let ( and* ) m n = both m n

    let ( >|= ) m f = map f m

    let ( let+ ) m f = map f m

    let ( and+ ) m n = both m n

    let ( >>~ ) m f = m >|= Response.value >>= f

    let ( *> ) p1 p2 = p1 >>= fun _ -> p2

    let embed lw = Lwt.(fun state -> lw >>= fun v -> return (state, Response v))

    let fail exn _state = Lwt.fail exn

    let catch try_ with_ state =
      Lwt.catch (fun () -> try_ () state) (fun exn -> with_ exn state)
  end

  module Endpoint = struct
    module Version = struct
      type t = Etag of string | Last_modified of string

      let of_headers headers =
        match C.Header.get headers "etag" with
        | Some etag -> Some (Etag etag)
        | None -> (
            match C.Header.get headers "last-modified" with
            | Some last -> Some (Last_modified last)
            | None -> None)

      let add_conditional_headers headers = function
        | None -> headers
        | Some (Etag etag) -> C.Header.add headers "If-None-Match" etag
        | Some (Last_modified time) ->
            C.Header.add headers "If-Modified-Since" time
    end

    type t = { uri : Uri.t; version : Version.t option }

    let empty = { uri = Uri.empty; version = None }

    let poll_after : (string, float) Hashtbl.t = Hashtbl.create 8

    let update_poll_after uri { C.Response.headers; _ } =
      let now = Time.now () in
      let poll_limit =
        match C.Header.get headers "x-poll-interval" with
        | Some interval -> now +. float_of_string interval
        | None -> now +. 60.
      in
      let uri_s = Uri.to_string uri in
      let t_0 = try Hashtbl.find poll_after uri_s with Not_found -> 0. in
      if t_0 < poll_limit then Hashtbl.replace poll_after uri_s poll_limit

    let poll_result uri ({ C.Response.headers; _ } as envelope) =
      let version = Version.of_headers headers in
      update_poll_after uri envelope;
      { uri; version }

    (* TODO: multiple polling threads need to queue *)
    let wait_to_poll uri =
      let now = Time.now () in
      let uri_s = Uri.to_string uri in
      let t_1 = try Hashtbl.find poll_after uri_s with Not_found -> 0. in
      Monad.embed
        (if now < t_1 then Time.sleep (t_1 -. now) else Lwt.return_unit)
  end

  module Stream = struct
    type 'a t = {
      restart : Endpoint.t -> 'a t option Monad.t;
      buffer : 'a list;
      refill : (unit -> 'a t Monad.t) option;
      endpoint : Endpoint.t;
    }

    type 'a parse = string -> 'a list Lwt.t

    let empty =
      {
        restart = (fun _endpoint -> Monad.return None);
        buffer = [];
        refill = None;
        endpoint = Endpoint.empty;
      }

    let rec next =
      Monad.(
        function
        | { buffer = []; refill = None; _ } -> return None
        | { buffer = []; refill = Some refill; _ } -> refill () >>= next
        | { buffer = h :: buffer; _ } as s ->
            return (Some (h, { s with buffer })))

    let map f s =
      let rec refill s () =
        Monad.(
          next s >>= function
          | None -> return empty
          | Some (v, s) -> (
              f v >>= function
              | [] -> refill s ()
              | buffer ->
                  return { s with restart; buffer; refill = Some (refill s) }))
      and restart endpoint =
        Monad.(
          s.restart endpoint >>= function
          | Some s ->
              return
                (Some { s with restart; buffer = []; refill = Some (refill s) })
          | None -> return None)
      in
      { s with restart; buffer = []; refill = Some (refill s) }

    let rec fold f a s =
      Monad.(
        next s >>= function
        | None -> return a
        | Some (v, s) -> f a v >>= fun a -> fold f a s)

    let rec find p s =
      Monad.(
        next s >>= function
        | None -> return None
        | Some (n, s) as c -> if p n then return c else find p s)

    let rec iter f s =
      Monad.(
        next s >>= function
        | None -> return ()
        | Some (v, s) -> f v >>= fun () -> iter f s)

    let to_list s =
      let rec aux lst s =
        Monad.(
          next s >>= function
          | None -> return (List.rev lst)
          | Some (v, s) -> aux (v :: lst) s)
      in
      aux [] s

    let of_list buffer = { empty with buffer; refill = None }

    let poll stream = stream.restart stream.endpoint

    let since stream version =
      {
        stream with
        endpoint = { stream.endpoint with Endpoint.version = Some version };
      }

    let version stream = stream.endpoint.Endpoint.version
  end

  type 'a parse = string -> 'a Lwt.t

  type 'a handler = (C.Response.t * string -> bool) * 'a

  module API = struct
    (* Use the highest precedence handler that matches the response. *)
    let rec handle_response redirects ((envelope, body) as response) =
      Lwt.(
        function
        | (p, handler) :: more ->
            if not (p response) then handle_response redirects response more
            else
              let bad_response exn body =
                return Monad.(error (Bad_response (exn, body)))
              in
              catch
                (fun () ->
                  handler response >>= fun r ->
                  return (Monad.response (Response.wrap ~redirects r)))
                (fun exn ->
                  catch
                    (fun () ->
                      catch
                        (fun () ->
                          let json = Yojson.Basic.from_string body in
                          log "response body:\n%s"
                            (Yojson.Basic.pretty_to_string json);
                          bad_response exn (`Json json))
                        (fun _exn -> bad_response exn (`Raw body)))
                    (fun _exn -> bad_response exn `None))
        | [] -> (
            let status = C.Response.status envelope in
            match status with
            | `Unprocessable_entity | `Gone | `Unauthorized | `Forbidden ->
                let message = Gitlab_j.message_of_string body in
                return Monad.(error (Semantic (status, message)))
            | _ -> return Monad.(error (Generic (envelope, body)))))

    (* Force chunked-encoding
     * to be disabled (to satisfy Github, which returns 411 Length Required
     * to a chunked-encoding POST request). *)
    let lwt_req { Monad.uri; meth; headers; body } =
      log "Requesting %s" (Uri.to_string uri);
      let body = CLB.of_string body in
      CL.call ~headers ~body ~chunked:false meth uri

    let max_redirects = 64

    let make_redirect target = function
      | `Moved_permanently -> Response.Permanent target
      | _ -> Response.Temporary target

    let rec request ?(redirects = []) ~rate ~token resp_handlers req =
      Lwt.(
        if List.length redirects > max_redirects then
          Lwt.fail
            (Message
               ( `Too_many_requests,
                 Gitlab_t.
                   {
                     message_message =
                       Printf.sprintf "ocaml-github exceeded max redirects %d"
                         max_redirects;
                     message_errors = [];
                   } ))
        else
          lwt_req req >>= fun (resp, body) ->
          let response_code = C.Response.status resp in
          log "Response code %s\n%!" (C.Code.string_of_status response_code);
          match response_code with
          | `Found | `Temporary_redirect | `Moved_permanently -> (
              match C.Header.get (C.Response.headers resp) "location" with
              | None ->
                  Lwt.fail
                    (Message
                       ( `Expectation_failed,
                         Gitlab_t.
                           {
                             message_message =
                               "ocaml-gitlab got redirect without location";
                             message_errors = [];
                           } ))
              | Some location_s ->
                  let location = Uri.of_string location_s in
                  let target = Uri.resolve "" req.Monad.uri location in
                  let redirect = make_redirect target response_code in
                  let redirects = redirect :: redirects in
                  let req = { req with Monad.uri = target } in
                  request ~redirects ~rate ~token resp_handlers req)
          | _ ->
              CLB.to_string body >>= fun body ->
              handle_response (List.rev redirects) (resp, body) resp_handlers)

    (* A simple response pattern that matches on HTTP code equivalence *)
    let code_handler ~expected_code handler =
      ((fun (res, _) -> C.Response.status res = expected_code), handler)

    (* Add the correct mime-type header and the authentication token. *)
    let realize_headers ~token ~headers =
      match token with
      | Some token -> C.Header.add_opt headers "PRIVATE-TOKEN" token
      | None -> C.Header.init ()

    let idempotent meth ?(rate = Core) ?headers ?token ?params ~fail_handlers
        ~expected_code ~uri fn state =
      Lwt.return
        ( state,
          Monad.(
            request ?token ?params
              {
                meth;
                uri;
                headers = realize_headers ~token ~headers;
                body = "";
              })
            (request ~rate ~token
               (code_handler ~expected_code fn :: fail_handlers)) )

    let just_body (_, (body : string)) : string Lwt.t = Lwt.return body

    let effectful meth ?(rate = Core) ?headers ?body ?token ?params
        ~fail_handlers ~expected_code ~uri fn =
      let body = match body with None -> "" | Some b -> b in
      let fn x = Lwt.(just_body x >>= fn) in
      let fail_handlers =
        List.map
          (fun (p, fn) -> (p, Lwt.(fun x -> just_body x >>= fn)))
          fail_handlers
      in
      fun state ->
        Lwt.return
          ( state,
            Monad.(
              request ?token ?params
                { meth; uri; headers = realize_headers ~token ~headers; body })
              (request ~rate ~token
                 (code_handler ~expected_code fn :: fail_handlers)) )

    let map_fail_handlers f fhs = List.map (fun (p, fn) -> (p, f fn)) fhs

    let get ?rate ?(fail_handlers = []) ?(expected_code = `OK) ?headers ?token
        ?params ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `GET ?rate ~fail_handlers ~expected_code ?headers ?token
        ?params ~uri
        Lwt.(fun x -> just_body x >>= fn)

    let rec next_link base =
      Cohttp.Link.(
        function
        | { context; arc = { Arc.relation; _ }; target } :: _
          when Uri.(equal context empty) && List.mem Rel.next relation ->
            Some (Uri.resolve "" base target)
        | _ :: rest -> next_link base rest
        | [] -> None)

    let stream_fail_handlers restart fhs =
      map_fail_handlers
        Lwt.(
          fun f (_envelope, body) ->
            f body >>= fun buffer ->
            return
              {
                Stream.restart;
                buffer;
                refill = None;
                endpoint = Endpoint.empty;
              })
        fhs

    let rec stream_next restart request uri fn endpoint (envelope, body) =
      Lwt.(
        let endpoint =
          match endpoint.Endpoint.version with
          | None -> Endpoint.poll_result uri envelope
          | Some _ -> endpoint
        in
        let refill =
          Some
            (fun () ->
              let links = Cohttp.(Header.get_links envelope.Response.headers) in
              match next_link uri links with
              | None -> Monad.return Stream.empty
              | Some uri ->
                  request ~uri (stream_next restart request uri fn endpoint))
        in
        fn body >>= fun buffer ->
        return { Stream.restart; buffer; refill; endpoint })

    let rec restart_stream ?rate ~fail_handlers ~expected_code ?headers ?token
        ?params fn endpoint =
      let restart =
        restart_stream ?rate ~fail_handlers ~expected_code ?headers ?token
          ?params fn
      in
      let first_request ~uri f =
        let not_mod_handler =
          code_handler ~expected_code:`Not_modified (fun (envelope, _) ->
              Endpoint.update_poll_after uri envelope;
              Lwt.return_none)
        in
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        let fail_handlers =
          map_fail_handlers
            Lwt.(fun f response -> f response >|= fun stream -> Some stream)
            fail_handlers
        in
        let fail_handlers = not_mod_handler :: fail_handlers in
        let f ((envelope, _) as response) =
          Lwt.(
            let endpoint = Endpoint.poll_result uri envelope in
            f response >|= fun stream -> Some { stream with Stream.endpoint })
        in
        let headers =
          match headers with None -> C.Header.init () | Some h -> h
        in
        let headers =
          Endpoint.(Version.add_conditional_headers headers endpoint.version)
        in
        Monad.(
          Endpoint.wait_to_poll uri >>= fun () ->
          idempotent ?rate `GET ~headers ?token ?params ~fail_handlers
            ~expected_code ~uri f)
      in
      let request ~uri f =
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        Monad.map Response.value
          (idempotent ?rate `GET ?headers ?token ?params ~fail_handlers
             ~expected_code ~uri f)
      in
      let uri = endpoint.Endpoint.uri in
      Monad.map Response.value
        (first_request ~uri (stream_next restart request uri fn endpoint))

    let get_stream (type a) ?rate
        ?(fail_handlers : a Stream.parse handler list = [])
        ?(expected_code : Cohttp.Code.status_code = `OK)
        ?(headers : Cohttp.Header.t option) ?(token : string option)
        ?(params : (string * string) list option) ~(uri : Uri.t)
        (fn : a Stream.parse) =
      let restart =
        restart_stream ?rate ~fail_handlers ~expected_code ?headers ?token
          ?params fn
      in
      let request ~uri f =
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        Monad.map Response.value
          (idempotent ?rate `GET ?headers ?token ?params ~fail_handlers
             ~expected_code ~uri f)
      in
      let endpoint = Endpoint.{ empty with uri } in
      let refill =
        Some
          (fun () -> request ~uri (stream_next restart request uri fn endpoint))
      in
      { Stream.restart; buffer = []; refill; endpoint }

    let post ?rate ?(fail_handlers = []) ~expected_code =
      effectful `POST ?rate ~fail_handlers ~expected_code

    let patch ?rate ?(fail_handlers = []) ~expected_code =
      effectful `PATCH ?rate ~fail_handlers ~expected_code

    let put ?rate ?(fail_handlers = []) ~expected_code ?headers ?body =
      let headers =
        match (headers, body) with
        | None, None -> Some (C.Header.init_with "content-length" "0")
        | Some h, None -> Some (C.Header.add h "content-length" "0")
        | _, Some _ -> headers
      in
      effectful `PUT ?rate ~fail_handlers ~expected_code ?headers ?body

    let delete ?rate ?(fail_handlers = []) ?(expected_code = `No_content)
        ?headers ?token ?params ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `DELETE ?rate ~fail_handlers ~expected_code ?headers ?token
        ?params ~uri
        Lwt.(fun x -> just_body x >>= fn)

    let set_user_agent user_agent state =
      Monad.(
        Lwt.return ({ state with user_agent = Some user_agent }, Response ()))

    let set_token token state =
      Monad.(Lwt.return ({ state with token = Some token }, Response ()))

    let string_of_message = Monad.string_of_message
  end

  module Token = struct
    type t = string

    let of_string x = x

    let to_string x = x
  end

  module URI = struct
    let api = Env.gitlab_uri

    let events = Uri.of_string (Printf.sprintf "%s/events" api)

    let user = Uri.of_string (Printf.sprintf "%s/users" api)

    let user_by_id ~id = Uri.of_string (Printf.sprintf "%s/users/%s" api id)

    let user_events ~id =
      Uri.of_string (Printf.sprintf "%s/users/%s/events" api id)

    let user_projects ~id =
      Uri.of_string (Printf.sprintf "%s/users/%s/projects" api id)

    let merge_requests = Uri.of_string (Printf.sprintf "%s/merge_requests" api)

    let project_commits project_id =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/repository/commits" api project_id)

    let project_commit project_id sha =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/repository/commits/%s" api project_id
           sha)

    let project_commit_statuses project_id sha =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/repository/commits/%s/statuses" api
           project_id sha)

    let project_commit_status project_id sha =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/statuses/%s" api project_id sha)

    let project_comments project_id sha =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/repository/commits/%s/comments" api
           project_id sha)

    let projects = Uri.of_string (Printf.sprintf "%s/projects" api)

    let projects_by_id project_id =
      Uri.of_string (Printf.sprintf "%s/projects/%i" api project_id)

    let project_branch project_id name =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/repository/branches/%s" api project_id
           name)

    let project_branches project_id =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/repository/branches" api project_id)

    let project_merge_requests ~id =
      Uri.of_string (Printf.sprintf "%s/projects/%i/merge_requests" api id)

    let project_merge_request ~id ~merge_request_iid =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/merge_requests/%s" api id
           merge_request_iid)

    let project_merge_request_participants ~id ~merge_request_iid =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/merge_requests/%s/participants" api id
           merge_request_iid)

    let project_merge_request_commits ~id ~merge_request_iid =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/merge_requests/%s/commits" api id
           merge_request_iid)

    let project_merge_request_changes ~id ~merge_request_iid =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/merge_requests/%s/changes" api id
           merge_request_iid)

    let project_events ~id =
      Uri.of_string (Printf.sprintf "%s/projects/%i/events" api id)

    let project_milestones ~project_id =
      Uri.of_string (Printf.sprintf "%s/projects/%i/milestones" api project_id)

    let project_milestone ~project_id ~milestone_id =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/milestones/%i" api project_id
           milestone_id)

    let group_projects ~id =
      Uri.of_string (Printf.sprintf "%s/groups/%s/projects" api id)

    let group_merge_requests ~id =
      Uri.of_string (Printf.sprintf "%s/groups/%s/merge_requests" api id)

    let group_milestones ~group_id =
      Uri.of_string (Printf.sprintf "%s/group/%i/milestones" api group_id)

    let group_milestone ~group_id ~milestone_id =
      Uri.of_string
        (Printf.sprintf "%s/group/%i/milestones/%i" api group_id milestone_id)

    let external_status_checks ~id =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/external_status_checks" api id)

    let external_status_check ~id ~check_id =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/external_status_checks/%i" api id
           check_id)

    let list_status_checks ~id ~merge_request_iid =
      Uri.of_string
        (Printf.sprintf "%s/projects/%i/merge_requests/%s/status_checks" api id
           merge_request_iid)

    let set_status_check ~id ~merge_request_iid =
      Uri.of_string
        (Printf.sprintf
           "%s/projects/%i/merge_requests/%s/status_check_responses" api id
           merge_request_iid)
  end

  (* Query Parameter helpers *)
  let state_param state uri =
    let show = function
      | `Opened -> "opened"
      | `Closed -> "closed"
      | `Locked -> "locked"
      | `Merged -> "merged"
    in
    match state with
    | None -> uri
    | Some state -> Uri.add_query_param' uri ("state", show state)

  let commit_state_param state uri =
    (* TODO This pattern along with the enum in lab.ml should be generic and
       derived off the gitlab.atd definition.
    *)
    let show = function
      | `Pending -> "pending"
      | `Running -> "running"
      | `Success -> "success"
      | `Failed -> "failed"
      | `Cancelled -> "cancelled"
    in
    Uri.add_query_param' uri ("state", show state)

  let action_param action uri =
    match action with
    | None -> uri
    | Some action -> Uri.add_query_param' uri ("action", action)

  let target_type_param target_type uri =
    match target_type with
    | None -> uri
    | Some target_type -> Uri.add_query_param' uri ("target_type", target_type)

  let milestone_param milestone uri =
    match milestone with
    | None -> uri
    | Some milestone -> Uri.add_query_param' uri ("milestone", milestone)

  let labels_param labels uri =
    match labels with
    | None | Some [] -> uri
    | Some labels ->
        Uri.add_query_param' uri ("labels", String.concat "," labels)

  let author_id_param author_id uri =
    match author_id with
    | None -> uri
    | Some author_id -> Uri.add_query_param' uri ("author_id", author_id)

  let author_username_param author_username uri =
    match author_username with
    | None -> uri
    | Some author_username ->
        Uri.add_query_param' uri ("author_username", author_username)

  let my_reaction_param my_reaction uri =
    match my_reaction with
    | None -> uri
    | Some my_reaction -> Uri.add_query_param' uri ("my_reaction", my_reaction)

  let scope_param scope uri =
    let show = function
      | `CreatedByMe -> "created_by_me"
      | `AssignedToMe -> "assigned_to_me"
      | `All -> "all"
    in
    match scope with
    | None -> uri
    | Some scope -> Uri.add_query_param' uri ("scope", show scope)

  let name_param name uri =
    match name with
    | None -> uri
    | Some name -> Uri.add_query_param' uri ("name", name)

  let description_param description uri =
    match description with
    | None -> uri
    | Some description -> Uri.add_query_param' uri ("description", description)

  let external_url_param external_url uri =
    match external_url with
    | None -> uri
    | Some external_url ->
        Uri.add_query_param' uri ("external_url", external_url)

  let owned_param owned uri =
    match owned with
    | None -> uri
    | Some owned -> Uri.add_query_param' uri ("owned", Bool.to_string owned)

  let search_param search uri =
    match search with
    | None -> uri
    | Some search -> Uri.add_query_param' uri ("search", search)

  let with_programming_language_param lang uri =
    match lang with
    | None -> uri
    | Some lang -> Uri.add_query_param' uri ("with_programming_language", lang)

  let ref_name_param ref_name uri =
    match ref_name with
    | None -> uri
    | Some ref_name -> Uri.add_query_param' uri ("ref_name", ref_name)

  let ref_param ref_name uri =
    match ref_name with
    | None -> uri
    | Some ref_name -> Uri.add_query_param' uri ("ref", ref_name)

  let stage_param stage uri =
    match stage with
    | None -> uri
    | Some stage -> Uri.add_query_param' uri ("stage", stage)

  let since_param since uri =
    match since with
    | None -> uri
    | Some since -> Uri.add_query_param' uri ("since", since)

  let until_param until uri =
    match until with
    | None -> uri
    | Some until -> Uri.add_query_param' uri ("until", until)

  let path_param path uri =
    match path with
    | None -> uri
    | Some path -> Uri.add_query_param' uri ("path", path)

  let all_param all uri =
    match all with
    | None -> uri
    | Some all -> Uri.add_query_param' uri ("all", Bool.to_string all)

  let stats_param stats uri =
    match stats with
    | None -> uri
    | Some stats -> Uri.add_query_param' uri ("stats", Bool.to_string stats)

  let note_param note uri = Uri.add_query_param' uri ("note", note)

  let line_param line uri =
    match line with
    | None -> uri
    | Some line -> Uri.add_query_param' uri ("line", Int.to_string line)

  let line_type_param line_type uri =
    let show = function `New -> "new" | `Old -> "old" in
    match line_type with
    | None -> uri
    | Some line_type -> Uri.add_query_param' uri ("line_type", show line_type)

  let target_url_param target_url uri =
    match target_url with
    | None -> uri
    | Some target_url -> Uri.add_query_param' uri ("target_url", target_url)

  let coverage_param coverage uri =
    match coverage with
    | None -> uri
    | Some coverage ->
        Uri.add_query_param' uri ("coverage", Float.to_string coverage)

  let pipeline_id_param pipeline_id uri =
    match pipeline_id with
    | None -> uri
    | Some pipeline_id ->
        Uri.add_query_param' uri ("pipeline_id", Int.to_string pipeline_id)

  let title_param title uri =
    match title with
    | None -> uri
    | Some title -> Uri.add_query_param' uri ("title", title)

  let milestone_state_param milestone_state uri =
    let show = function `Active -> "active" | `Closed -> "closed" in
    match milestone_state with
    | None -> uri
    | Some milestone_state ->
        Uri.add_query_param' uri ("milestone_state", show milestone_state)

  let due_date_param due_date uri =
    match due_date with
    | None -> uri
    | Some due_date -> Uri.add_query_param' uri ("due_date", due_date)

  let start_date_param start_date uri =
    match start_date with
    | None -> uri
    | Some start_date -> Uri.add_query_param' uri ("start_date", start_date)

  module Event = struct
    open Lwt

    let all ~token () =
      let uri = URI.events in
      API.get ~token ~uri (fun body -> return (Gitlab_j.events_of_string body))
  end

  module User = struct
    open Lwt

    let by_id ~id () =
      let uri = URI.user_by_id ~id in
      API.get ~uri (fun body -> return (Gitlab_j.user_of_string body))

    let by_name ~name () =
      let params = [ ("username", name) ] in
      API.get ~uri:URI.user ~params (fun body ->
          return (Gitlab_j.users_of_string body))

    let projects ~id () =
      let uri = URI.user_projects ~id in
      API.get ~uri (fun body -> return (Gitlab_j.projects_short_of_string body))

    let merge_requests ~token ?state ?milestone ?labels ?author ?author_username
        ?my_reaction ?scope () =
      let uri =
        URI.merge_requests |> state_param state |> milestone_param milestone
        |> labels_param labels |> author_id_param author
        |> author_username_param author_username
        |> my_reaction_param my_reaction
        |> scope_param scope
      in
      API.get_stream ~token ~uri (fun body ->
          return (Gitlab_j.merge_requests_of_string body))

    let events ~token ~id ?action ?target_type () =
      let uri =
        URI.user_events ~id |> action_param action
        |> target_type_param target_type
      in
      API.get ~token ~uri (fun body -> return (Gitlab_j.events_of_string body))
  end

  module Project = struct
    open Lwt

    let create ~token ~name ?description () =
      let uri =
        URI.projects |> description_param description |> name_param (Some name)
      in
      API.post ~token ~uri ~expected_code:`Created (fun s ->
          Lwt.return (Gitlab_j.project_short_of_string s))

    let by_name ?token ~owner ~name () =
      let uri =
        URI.user_projects ~id:owner |> fun uri ->
        Uri.add_query_param' uri ("search", name)
      in
      API.get ?token ~uri (fun body ->
          return (Gitlab_j.projects_short_of_string body))

    let by_id ?token ~project_id () =
      let uri = URI.projects_by_id project_id in
      let fail_handlers =
        [ API.code_handler ~expected_code:`Not_found (fun _ -> return None) ]
      in
      API.get ?token ~uri ~fail_handlers (fun body ->
          return (Some (Gitlab_j.project_short_of_string body)))

    let merge_requests ?token ?state ?milestone ?labels ?author ?author_username
        ?my_reaction ?scope ~id () =
      let uri =
        URI.project_merge_requests ~id
        |> state_param state |> milestone_param milestone |> labels_param labels
        |> author_id_param author
        |> author_username_param author_username
        |> my_reaction_param my_reaction
        |> scope_param scope
      in
      API.get_stream ?token ~uri (fun body ->
          return (Gitlab_j.merge_requests_of_string body))

    let merge_request ?token ~project_id ~merge_request_iid () =
      let uri = URI.project_merge_request ~id:project_id ~merge_request_iid in
      API.get ?token ~uri (fun body ->
          return (Gitlab_j.merge_request_of_string body))

    let merge_request_participants ?token ~project_id ~merge_request_iid () =
      let uri =
        URI.project_merge_request_participants ~id:project_id ~merge_request_iid
      in
      API.get ?token ~uri (fun body -> return (Gitlab_j.users_of_string body))

    let merge_request_commits ?token ~project_id ~merge_request_iid () =
      let uri =
        URI.project_merge_request_commits ~id:project_id ~merge_request_iid
      in
      API.get ?token ~uri (fun body -> return (Gitlab_j.commits_of_string body))

    let merge_request_changes ?token ~project_id ~merge_request_iid () =
      let uri =
        URI.project_merge_request_changes ~id:project_id ~merge_request_iid
      in
      API.get ?token ~uri (fun body -> return (Gitlab_j.changes_of_string body))

    let events ~token ~project_id ?action ?target_type () =
      let uri =
        URI.project_events ~id:project_id
        |> action_param action
        |> target_type_param target_type
      in
      API.get ~token ~uri (fun body -> return (Gitlab_j.events_of_string body))

    let all_projects ~token ?owned ?search ?with_programming_language () =
      let uri =
        URI.projects |> owned_param owned |> search_param search
        |> with_programming_language_param with_programming_language
      in
      API.get_stream ~token ~uri (fun body ->
          return (Gitlab_j.project_shorts_of_string body))

    module Commit = struct
      let commits ?token ~project_id ?ref_name ?since ?until ?path ?all () =
        let uri =
          URI.project_commits project_id
          |> ref_name_param ref_name |> since_param since |> until_param until
          |> path_param path |> all_param all
        in
        API.get_stream ?token ~uri (fun body ->
            return (Gitlab_j.commits_of_string body))

      let commit ?token ~project_id ~sha ?stats () =
        let uri = URI.project_commit project_id sha |> stats_param stats in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.commit_of_string body))

      let comments ?token ~project_id ~sha () =
        let uri = URI.project_comments project_id sha in
        API.get_stream ?token ~uri (fun body ->
            return (Gitlab_j.commit_comments_of_string body))

      let comment ?token ~project_id ~sha ~note ?path ?line ?line_type () =
        let uri =
          URI.project_comments project_id sha
          |> note_param note |> path_param path |> line_param line
          |> line_type_param line_type
        in
        API.post ?token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.commit_commented_of_string body))

      let statuses ?token ~project_id ~sha ?ref_name ?stage ?name ?all () =
        let uri =
          URI.project_commit_statuses project_id sha
          |> ref_param ref_name |> stage_param stage |> name_param name
          |> all_param all
        in
        API.get_stream ?token ~uri (fun body ->
            return (Gitlab_j.commit_statuses_of_string body))

      let status ~token ~project_id ~sha ~state ?ref_name ?name ?target_url
          ?description ?coverage ?pipeline_id () =
        let uri =
          URI.project_commit_status project_id sha
          |> commit_state_param state |> ref_param ref_name |> name_param name
          |> target_url_param target_url
          |> description_param description
          |> coverage_param coverage
          |> pipeline_id_param pipeline_id
        in
        API.post ~token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.commit_status_of_string body))
    end

    module Branch = struct
      let branches ?token ~project_id ?search () =
        let uri = URI.project_branches project_id |> search_param search in
        API.get_stream ?token ~uri (fun body ->
            return (Gitlab_j.branches_full_of_string body))

      let branch ?token ~project_id ~branch () =
        let uri = URI.project_branch project_id branch in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.branch_full_of_string body))

      let create ~token ~project_id ~branch ~ref () =
        let uri =
          URI.project_branches project_id |> fun x ->
          Uri.add_query_params' x [ ("branch", branch); ("ref", ref) ]
        in
        API.post ~token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.branch_full_of_string body))

      let delete ~token ~project_id ~branch () =
        let uri = URI.project_branch project_id branch in
        API.delete ~token ~uri ~expected_code:`No_content (fun _ -> return ())

      let delete_merged ~token ~project_id () =
        let uri = URI.project_branches project_id in
        API.delete ~token ~uri ~expected_code:`No_content (fun _ -> return ())
    end

    module ExternalStatusCheck = struct
      let list_for_merge_request ~token ~project_id ~merge_request_iid () =
        let uri = URI.list_status_checks ~id:project_id ~merge_request_iid in
        API.get ~token ~uri (fun body ->
            return (Gitlab_j.status_checks_of_string body))

      let set_status ~token ~project_id ~merge_request_iid ~sha
          ~external_status_check_id () =
        let uri =
          URI.set_status_check ~id:project_id ~merge_request_iid |> fun uri ->
          Uri.add_query_params' uri
            [
              ("sha", sha);
              ("external_status_check_id", external_status_check_id);
            ]
        in
        API.post ~token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.external_status_check_of_string body))

      let checks ~token ~project_id () =
        let uri = URI.external_status_checks ~id:project_id in
        API.get ~token ~uri (fun body ->
            return (Gitlab_j.external_status_checks_of_string body))

      let create ~token ~project_id ~name ~external_url ?protected_branch_ids:_
          () =
        let uri =
          URI.external_status_checks ~id:project_id |> fun uri ->
          Uri.add_query_params' uri
            [
              ("name", name);
              ("external_url", external_url);
              (* ; ("protected_branch_ids", protected_branch_ids) *)
            ]
        in
        API.post ~token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.external_status_check_of_string body))

      let delete ~token ~project_id ~rule_id () =
        let uri = URI.external_status_check ~id:project_id ~check_id:rule_id in
        API.delete ~token ~uri (fun _ -> return ())

      let update ~token ~project_id ~rule_id ?name ?external_url
          ?protected_branch_ids:_ () =
        let uri =
          URI.external_status_check ~id:project_id ~check_id:rule_id
          |> name_param name
          |> external_url_param external_url
          (* |> protected_branch_ids_param protected_branch_ids *)
        in
        API.put ~token ~uri ~expected_code:`OK (fun body ->
            return (Gitlab_j.external_status_check_of_string body))
    end

    module Milestone = struct
      let milestones ?token ~project_id ?state ?title ?search () =
        let uri =
          URI.project_milestones ~project_id
          |> title_param title
          |> milestone_state_param state
          |> search_param search
        in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.milestones_of_string body))

      let milestone ?token ~project_id ~milestone_id () =
        let uri = URI.project_milestone ~project_id ~milestone_id in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.milestone_of_string body))

      let create ~token ~project_id ~title ?description ?due_date ?start_date ()
          =
        let uri =
          URI.project_milestones ~project_id
          |> title_param (Some title)
          |> description_param description
          |> due_date_param due_date
          |> start_date_param start_date
        in
        API.post ~token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.milestone_of_string body))

      let update ~token ~project_id ~milestone_id ?title ?description ?due_date
          ?start_date ?state_event () =
        let uri =
          URI.project_milestone ~project_id ~milestone_id
          |> title_param title
          |> description_param description
          |> due_date_param due_date
          |> start_date_param start_date
          |> milestone_state_param state_event
        in
        API.put ~token ~uri ~expected_code:`OK (fun body ->
            return (Gitlab_j.milestone_of_string body))

      let delete ~token ~project_id ~milestone_id () =
        let uri = URI.project_milestone ~project_id ~milestone_id in
        API.delete ~token ~uri ~expected_code:`No_content (fun _ -> return ())
    end
  end

  module Group = struct
    open Lwt

    module Project = struct
      let by_name ?token ~owner ~name () =
        let uri =
          URI.group_projects ~id:owner |> fun uri ->
          Uri.add_query_param' uri ("search", name)
        in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.projects_short_of_string body))
    end

    let merge_requests ?token ?state ?milestone ?labels ?author ?author_username
        ?my_reaction ?scope ~id () =
      let uri =
        URI.group_merge_requests ~id
        |> state_param state |> milestone_param milestone |> labels_param labels
        |> author_id_param author
        |> author_username_param author_username
        |> my_reaction_param my_reaction
        |> scope_param scope
      in
      API.get_stream ?token ~uri (fun body ->
          return (Gitlab_j.merge_requests_of_string body))

    module Milestone = struct
      let milestones ?token ~group_id ?state ?title ?search () =
        let uri =
          URI.group_milestones ~group_id
          |> title_param title
          |> milestone_state_param state
          |> search_param search
        in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.milestones_of_string body))

      let milestone ?token ~group_id ~milestone_id () =
        let uri = URI.group_milestone ~group_id ~milestone_id in
        API.get ?token ~uri (fun body ->
            return (Gitlab_j.milestone_of_string body))

      let create ~token ~group_id ~title ?description ?due_date ?start_date () =
        let uri =
          URI.group_milestones ~group_id
          |> title_param (Some title)
          |> description_param description
          |> due_date_param due_date
          |> start_date_param start_date
        in
        API.post ~token ~uri ~expected_code:`Created (fun body ->
            return (Gitlab_j.milestone_of_string body))

      let update ~token ~group_id ~milestone_id ?title ?description ?due_date
          ?start_date ?state_event () =
        let uri =
          URI.group_milestone ~group_id ~milestone_id
          |> title_param title
          |> description_param description
          |> due_date_param due_date
          |> start_date_param start_date
          |> milestone_state_param state_event
        in
        API.put ~token ~uri ~expected_code:`OK (fun body ->
            return (Gitlab_j.milestone_of_string body))

      let delete ~token ~group_id ~milestone_id () =
        let uri = URI.group_milestone ~group_id ~milestone_id in
        API.delete ~token ~uri ~expected_code:`No_content (fun _ -> return ())
    end
  end
end
