open Cmdliner
open Printf
open Config

module CommandLine = struct
  let owner_id =
    let doc = "Gitlab Owner Id" in
    Arg.(
      required
      & opt (some string) None
      & info [ "o"; "owner" ] ~docv:"OWNER" ~doc)

  let owner_name =
    let doc = "Gitlab Ownername" in
    Arg.(
      required
      & opt (some string) None
      & info [ "n"; "owner-name" ] ~docv:"OWNER_NAME" ~doc)

  let api =
    let doc = " The GitLab API endpoint to send the HTTP request to" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ENDPOINT" ~doc)

  let project_id =
    let doc = "Project Name" in
    Arg.(
      required
      & opt (some int) None
      & info [ "p"; "project-id" ] ~docv:"PROJECT_ID" ~doc)

  let merge_request_id =
    let doc = "Merge Request Id" in
    Arg.(
      required
      & opt (some string) None
      & info [ "m"; "merge-request-id" ] ~docv:"MERGE_REQUEST_ID" ~doc)

  let project_name =
    let doc = "Project name" in
    Arg.(required & opt (some string) None & info ["p"; "project-name"] ~docv:"PROJECT_NAME" ~doc)
end

let user_cmd =
  let user_list user () =
    let cmd =
      let open Gitlab in
      let open Monad in
      User.by_id ~id:user () >>~ fun user ->
      return @@ printf "%s\n" user.Gitlab_t.user_username
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  (Term.(pure user_list $ CommandLine.owner_id $ pure ()), Term.info "user-list")

let user_name_cmd =
  let user_list name () =
    let cmd name =
      let open Gitlab in
      let open Monad in
      User.by_name ~name () >>~ fun users ->
      return
      @@ List.iter
           (fun user ->
             printf "%s:%i\n" user.Gitlab_t.user_short_username
               user.Gitlab_t.user_short_id)
           users
    in
    Lwt_main.run @@ Gitlab.Monad.run (cmd name)
  in
  ( Term.(pure user_list $ CommandLine.owner_name $ pure ()),
    Term.info "user-name" )

let user_projects_cmd =
  let user_projects_list id () =
    let cmd =
      let open Gitlab in
      let open Monad in
      User.projects ~id () >>~ fun projects ->
      return
      @@ List.iter
           (fun project -> printf "%s\n" project.Gitlab_t.project_short_name)
           projects
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  ( Term.(pure user_projects_list $ CommandLine.owner_id $ pure ()),
    Term.info "user-projects" )

let user_events_cmd =
  let user_projects_list id () =
    let cmd =
      let open Gitlab in
      let open Monad in
      User.events ~token:access_token ~id () >>~ fun events ->
      return @@ printf "%s\n" (Gitlab_j.string_of_events events)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  ( Term.(pure user_projects_list $ CommandLine.owner_id $ pure ()),
    Term.info "user-events" )

let merge_requests_cmd =
  let merge_requests_list () =
    let cmd =
      let open Gitlab in
      let open Monad in
      let* mr = return (User.merge_requests ~token:access_token ()) in
      Stream.iter
        (fun merge_request ->
          printf "#%i %s\n" merge_request.Gitlab_t.merge_request_id
            merge_request.Gitlab_t.merge_request_title;
          return ())
        mr
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  (Term.(pure merge_requests_list $ pure ()), Term.info "merge-requests")

let status_checks_cmd =
  let status_checks project_id () =
    let cmd =
      let open Gitlab in
      let open Monad in
      Project.ExternalStatusCheck.checks ~token:access_token ~project_id ()
      >>~ fun x ->
      return
      @@ List.iter
           (fun check ->
             printf "%s\t%s\t%i\n" check.Gitlab_t.external_status_check_name
               check.Gitlab_t.external_status_check_external_url
               check.Gitlab_t.external_status_check_id)
           x
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  ( Term.(pure status_checks $ CommandLine.project_id $ pure ()),
    Term.info "status-checks" )

let project_create_cmd =
  let project_create name ?description () =
    let cmd =
      let open Gitlab in
      let open Monad in
      Project.create ~token:access_token ~name ~description () *>
      User.projects
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  ( Term.(pure project_create $ CommandLine.name $ CommandLine.description $ pure ()), Term.info "project-create")

let api_cmd =
  let api uri_str () =
    let cmd =
      let open Gitlab in
      let open Monad in
      let uri = Uri.of_string uri_str in
      API.get ~uri (fun body -> Lwt.return (Yojson.Basic.from_string body))
      >>~ fun json -> return @@ printf "%s" (Yojson.Basic.pretty_to_string json)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  (Term.(pure api $ CommandLine.api $ pure ()), Term.info "api")

let default_cmd =
  let doc = "make git easier with GitLab" in
  ( Term.(ret (pure (`Help (`Pager, None)))),
    let man =
      [
        `S "DESCRIPTION";
        `P
          "Lab is a tool that wraps git in order to extend it with extra \
           functionality that makes it better when working with GitLab.";
        `S "BUGS";
        `P "<https://github.com/tmcgilchrist/ocaml-gitlab/issues>";
        `S "AUTHORS";
        `P "<https://github.com/tmcgilchrist/ocaml-gitlab>";
      ]
    in
    Term.info "lab" ~version:"0.1" ~doc ~man )

let cmds =
  [
    user_cmd;
    user_name_cmd;
    user_projects_cmd;
    merge_requests_cmd;
    status_checks_cmd;
    user_events_cmd;
    api_cmd;
    project_create_cmd;
  ]

let () =
  match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
