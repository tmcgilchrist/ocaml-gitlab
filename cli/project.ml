open Cmdliner
open Printf
open Config

let envs = Gitlab.Env.envs

let project_id =
  let doc = "Project Id" in
  Arg.(
    required
    & opt (some int) None
    & info [ "p"; "project-id" ] ~docv:"PROJECT_ID" ~doc)

let project_name =
  let doc = "The repository name on GitLab." in
  Arg.(
    required & pos 0 (some string) None & info [] ~docv:"PROJECT_NAME" ~doc)

let project_description =
  let doc = "A short description of the GitLab repository." in
  Arg.(
    required
    & opt (some string) None
    & info [ "d"; "description" ] ~docv:"PROJECT_DESCRIPTION" ~doc)

let status_checks_cmd config =
  let status_checks project_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      Project.ExternalStatusCheck.checks ~token:config.token ~project_id ()
      >|~ fun x ->
      List.iter
        (fun check ->
          printf "%s\t%s\t%i\n" check.Gitlab_t.external_status_check_name
            check.Gitlab_t.external_status_check_external_url
            check.Gitlab_t.external_status_check_id)
        x
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List external status checks." in
  let info = Cmd.info ~envs ~doc "status-checks" in
  let term = Term.(const status_checks $ project_id) in
  Cmd.v info term

let project_create_cmd config =
  let project_create name description =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      Project.create ~token:config.token ~name ~description () >|~ fun p ->
      printf "%s\n" (Yojson.Basic.prettify (Gitlab_j.string_of_project_short p))
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Creates a new project owned by the authenticated user." in
  let info = Cmd.info ~envs ~doc "project-create" in
  let term = Term.(const project_create $ project_name $ project_description) in
  Cmd.v info term

let project_branches_cmd config =
  let project_branches project_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let* branches =
        return @@ Project.Branch.branches ~token:config.token ~project_id ()
      in
      Stream.iter
        (fun branch -> return @@ printf "%s\n" branch.Gitlab_t.branch_full_name)
        branches
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List branches for a project." in
  let info = Cmd.info ~envs ~doc "branch" in
  let term = Term.(const project_branches $ project_id) in
  Cmd.v info term

let project_events_cmd config =
  let project_events project_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      Project.events ~token:config.token ~project_id () >|~ fun events ->
      printf "%s\n" (Yojson.Basic.prettify @@ Gitlab_j.string_of_events events)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List all project events." in
  let info = Cmd.info ~envs ~doc "events" in
  let term =  Term.(const project_events $ project_id) in
  Cmd.v info term

let group_name = "project"

let cmd config =
  let doc = "Create, clone, fork, and view projects." in
  let default = Term.(ret (const (`Help (`Pager, Some group_name)))) in
  let man = [ ] in
  let info = Cmd.info ~envs group_name ~doc ~man in
  Cmd.group ~default info
    [ project_branches_cmd config;
      project_create_cmd config;
      status_checks_cmd config;
      project_events_cmd config;
    ]
