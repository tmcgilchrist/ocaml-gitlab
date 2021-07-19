let user_agent = "ocaml-gitlab"

module Make(Time: Gitlab_s.Time)(CL : Cohttp_lwt.S.Client) 
= struct

  let log_active = ref true (** TODO Configure this! *)

  let log fmt =
    Printf.ksprintf (fun s ->
        match !log_active with
        | false -> ()
        | true  -> prerr_endline (">>> GitLab: " ^ s)) fmt

  type rate = Core 

  let string_of_message message =
    message.Gitlab_t.message_message^
      Gitlab_t.(List.fold_left
                  (fun s { error_resource; error_field; error_code; error_message; } ->
                    let error_field = match error_field with
                      | None -> "\"\""
                      | Some x -> x
                    in
                    let error_message = match error_message with
                      | None -> "\"\""
                      | Some x -> x
                    in
                    Printf.sprintf
                      "%s\n> Resource type: %s\n  Field: %s\n  Code: %s\n  Message: %s"
                      s error_resource error_field error_code error_message
                  )
                  "" message.Gitlab_t.message_errors)

  exception Message of Cohttp.Code.status_code * Gitlab_t.message

  module Response = struct
    type redirect =
      | Temporary of Uri.t
      | Permanent of Uri.t
    type 'a t = < value : 'a; redirects : redirect list >

                                            let value r = r#value

    let redirects r = r#redirects

    let rec final_resource = function
      | [] -> None
      | (Permanent uri)::rest -> perm_resource uri rest
      | (Temporary uri)::rest -> temp_resource uri rest
    and perm_resource uri = function
      | [] -> Some (Permanent uri)
      | (Permanent uri)::rest -> perm_resource uri rest
      | (Temporary uri)::rest -> temp_resource uri rest
    and temp_resource uri = function
      | [] -> Some (Temporary uri)
      | (Temporary uri | Permanent uri)::rest -> temp_resource uri rest

    let wrap : ?redirects:redirect list -> 'a -> 'a t =
      fun ?(redirects=[]) v -> object
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
    * be retried within the monad, or a permanent failure returned *)
    type error =
      | Generic of (C.Response.t * string)
      | Semantic of C.Code.status_code * Gitlab_t.message
      | Bad_response of exn * [ `None | `Json of Yojson.Basic.t | `Raw of string ]
    type request = {
      meth: C.Code.meth; uri: Uri.t;
      headers: C.Header.t; body: string;
    }

    type state = {
      user_agent: string option;
      token: string option
    }
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
      | Semantic (_,message) ->
        Lwt.return ("GitLabg API error: "^string_of_message message)
      | Bad_response (exn,j) ->
        Lwt.return (sprintf "Bad response: %s\n%s"
          (Printexc.to_string exn)
          (match j with
           |`None -> "<none>"
           |`Raw r -> sprintf "Raw body:\n%s" r
           |`Json j -> sprintf "JSON body:\n%s" (Yojson.Basic.pretty_to_string j)))

    let error err = Err err
    let response r = Response r
    let request ?token:_ ?(params=[]) ({ uri; _ } as req) reqfn =
      let uri = Uri.add_query_params' uri params in
      Request ({req with uri}, reqfn)

    let prepare_headers state headers =
      (* Add User-Agent *)
      let headers =
        C.Header.prepend_user_agent
          headers
          (user_agent^" "^C.Header.user_agent)
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
      { req with
        headers=prepare_headers state req.headers;
      }

    let rec bind fn x = fun state -> x state >>= function
      | state, Request (req, reqfn) ->
        reqfn (prepare_request state req)
        >>= fun r ->
        bind fn (fun state -> Lwt.return (state, r)) state
      | state, Response r -> fn r state
      | state, ((Err _) as x) -> Lwt.return (state, x)

    let return r = fun state -> Lwt.return (state, Response r)
    let map f m = bind (fun x -> return (f x)) m

    let initial_state = {user_agent=None; token=None}

    let run th = bind return th initial_state >>= function
      | _, Request (_,_) -> Lwt.fail (Failure "Impossible: can't run unapplied request")
      | _, Response r -> Lwt.return r
      | _, Err (Semantic (status,msg)) -> Lwt.(fail (Message (status,msg)))
      | _, Err e -> Lwt.(error_to_string e >>= fun err -> fail (Failure err))

    let (>>=) m f = bind f m
    let (>|=) m f = map f m
    let (>>~) m f = m >|= Response.value >>= f

    let embed lw =
      Lwt.(fun state -> lw >>= (fun v -> return (state, Response v)))

    let fail exn _state = Lwt.fail exn

    let catch try_ with_ state =
      Lwt.catch (fun () -> try_ () state) (fun exn -> with_ exn state)
  end

  type 'a parse = string -> 'a Lwt.t
  type 'a handler = (C.Response.t * string -> bool) * 'a

  module API = struct
    (* Use the highest precedence handler that matches the response. *)
    let rec handle_response redirects (envelope,body as response) = Lwt.(
      function
      | (p, handler)::more ->
        if not (p response) then handle_response redirects response more
        else
          let bad_response exn body = return (Monad.(error (Bad_response (exn,body)))) in
          catch (fun () ->
            handler response
            >>= fun r ->
            return (Monad.response (Response.wrap ~redirects r))
          ) (fun exn ->
            catch (fun () ->
              catch (fun () ->
                let json = Yojson.Basic.from_string body in
                log "response body:\n%s" (Yojson.Basic.pretty_to_string json);
                bad_response exn (`Json json)
              ) (fun _exn -> bad_response exn (`Raw body))
            ) (fun _exn -> bad_response exn `None)
          )
      | [] ->
        let status = C.Response.status envelope in
        match status with
        | `Unprocessable_entity | `Gone | `Unauthorized | `Forbidden ->
          let message = Gitlab_j.message_of_string body in
          return Monad.(error (Semantic (status,message)))
        | _ ->
          return Monad.(error (Generic (envelope, body)))
    )

    (* Force chunked-encoding
     * to be disabled (to satisfy Github, which returns 411 Length Required
     * to a chunked-encoding POST request). *)
    let lwt_req {Monad.uri; meth; headers; body} =
      log "Requesting %s" (Uri.to_string uri);
      let body = CLB.of_string body in
      CL.call ~headers ~body ~chunked:false meth uri

    let max_redirects = 64
    let make_redirect target = function
      | `Moved_permanently -> Response.Permanent target
      | _ -> Response.Temporary target

    let rec request ?(redirects=[]) ~rate ~token resp_handlers req = Lwt.(
      if List.length redirects > max_redirects
      then Lwt.fail (Message (`Too_many_requests, Gitlab_t.{
        message_message = Printf.sprintf
            "ocaml-github exceeded max redirects %d" max_redirects;
        message_errors = [];
      }))
      else
        lwt_req req
        >>= fun (resp, body) ->
        let response_code = C.Response.status resp in
        log "Response code %s\n%!" (C.Code.string_of_status response_code);
        match response_code with
        | `Found | `Temporary_redirect | `Moved_permanently -> begin
            match C.Header.get (C.Response.headers resp) "location" with
            | None -> Lwt.fail (Message (`Expectation_failed, Gitlab_t.{
              message_message = "ocaml-gitlab got redirect without location";
              message_errors = [];
            }))
            | Some location_s ->
              let location = Uri.of_string location_s in
              let target = Uri.resolve "" req.Monad.uri location in
              let redirect = make_redirect target response_code in
              let redirects = redirect::redirects in
              let req = { req with Monad.uri = target } in
              request ~redirects ~rate ~token resp_handlers req
          end
        | _ ->
          CLB.to_string body >>= fun body ->
          handle_response (List.rev redirects) (resp,body) resp_handlers
    )

    (* A simple response pattern that matches on HTTP code equivalence *)
    let code_handler ~expected_code handler =
      (fun (res,_) -> C.Response.status res = expected_code), handler

    (* Add the correct mime-type header and the authentication token. *)
    let realize_headers
        ~token
        ~headers =
      match token with
      | Some token -> C.Header.add_opt headers "PRIVATE-TOKEN" token
      | None -> C.Header.init ()

    let idempotent meth
        ?(rate=Core) ?headers ?token ?params ~fail_handlers ~expected_code ~uri
        fn =
      fun state -> Lwt.return
        (state,
         (Monad.(request ?token ?params
                   {meth; uri; headers=realize_headers ~token ~headers; body=""})
            (request ~rate ~token
               ((code_handler ~expected_code fn)::fail_handlers))))

    let just_body (_,(body:string)):string Lwt.t = Lwt.return body

    let effectful meth
        ?(rate=Core) ?headers ?body ?token ?params
        ~fail_handlers ~expected_code ~uri fn =
      let body = match body with None -> ""| Some b -> b in
      let fn x = Lwt.(just_body x >>= fn) in
      let fail_handlers = List.map (fun (p,fn) ->
        p,Lwt.(fun x -> just_body x >>= fn)
      ) fail_handlers in
      fun state -> Lwt.return
        (state,
        (Monad.(request ?token ?params
                  {meth; uri; headers=realize_headers ~token ~headers; body })
           (request ~rate ~token
              ((code_handler ~expected_code fn)::fail_handlers))))

    let map_fail_handlers f fhs = List.map (fun (p,fn) ->
      p, f fn;
    ) fhs

    let get ?rate
        ?(fail_handlers=[]) ?(expected_code=`OK) ?headers
        ?token ?params ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `GET ?rate ~fail_handlers ~expected_code
        ?headers ?token ?params
        ~uri Lwt.(fun x -> just_body x >>= fn)

    let post ?rate ?(fail_handlers=[]) ~expected_code =
      effectful `POST ?rate ~fail_handlers ~expected_code

    let patch ?rate ?(fail_handlers=[]) ~expected_code =
      effectful `PATCH ?rate ~fail_handlers ~expected_code

    let put ?rate ?(fail_handlers=[]) ~expected_code ?headers ?body =
      let headers = match headers, body with
        | None, None -> Some (C.Header.init_with "content-length" "0")
        | Some h, None -> Some (C.Header.add h "content-length" "0")
        | _, Some _ -> headers
      in
      effectful `PUT ?rate ~fail_handlers ~expected_code ?headers ?body

    let delete ?rate
        ?(fail_handlers=[]) ?(expected_code=`No_content) ?headers ?token ?params
        ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `DELETE ?rate
        ~fail_handlers ~expected_code ?headers ?token ?params
        ~uri Lwt.(fun x -> just_body x >>= fn)

    let set_user_agent user_agent = fun state ->
      Monad.(Lwt.return ({state with user_agent=Some user_agent}, Response ()))

    let set_token token = fun state ->
      Monad.(Lwt.return ({state with token=Some token}, Response ()))

    let string_of_message = Monad.string_of_message
  end

  module Token = struct
    type t = string

    let of_string x = x
    let to_string x = x
  end

  module URI = struct

    let api = "https://gitlab.com/api/v4"

    let user = Uri.of_string (Printf.sprintf "%s/users" api)
    let user_by_id ~id = Uri.of_string (Printf.sprintf "%s/users/%s" api id)
    let user_projects ~id = Uri.of_string (Printf.sprintf "%s/users/%s/projects" api id)
    let merge_requests = Uri.of_string (Printf.sprintf "%s/merge_requests" api)
  end

  module User = struct
    open Lwt

    let by_id ~id () =
      let uri = URI.user_by_id ~id in
      API.get ~uri (fun body -> return (Gitlab_j.user_of_string body))

    let by_name ~name () =
      let params = [("username", name)] in
      API.get ~uri:URI.user ~params (fun body -> return (Gitlab_j.users_of_string body))

    let projects ~id () =
      let uri = URI.user_projects ~id in
      API.get ~uri (fun body -> return (Gitlab_j.projects_of_string body))

    let merge_requests ~token () =
      let uri = URI.merge_requests in
      API.get ~token ~uri (fun body -> return (Gitlab_j.merge_requests_of_string body))
  end
end
