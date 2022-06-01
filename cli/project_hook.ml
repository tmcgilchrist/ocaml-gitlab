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

let hook_id =
  let doc = "Hook Id" in
  Arg.(
    required
    & opt (some int) None
    & info [ "i"; "hook-id" ] ~docv:"HOOK_ID" ~doc)
let url =
  let doc = "Webhook URL" in
  Arg.(
    required
    & opt (some string) None
    & info [ "u"; "url" ] ~docv:"URL" ~doc)

let project_hook_create_cmd config =
  let project_hook_create project_id url =
    let cmd =
      let open Gitlab in
      let data : Gitlab_t.create_project_hook = {
        id = None;
        url = url;
        enable_ssl_verification = Some true;
        push_events = Some true;
        confidential_issues_events = None;
        confidential_note_events = None;
        deployment_events = None;
        issues_events = None;
        job_events = None;
        merge_requests_events = None;
        note_events = None;
        pipeline_events = None;
        push_events_branch_filter = None;
        releases_events = None;
        tag_push_events = None;
        repository_update_events = None;
        wiki_page_events = None;
        token = None;
      } in
      let open Monad in
      let config = config () in
      Project.Hook.create ~token:config.token ~project_id data () >|~ fun p ->
        printf "%s\n" (Yojson.Basic.prettify @@ Gitlab_j.string_of_project_hook p)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Create project hook." in
  let info = Cmd.info ~envs ~doc "create" in
  let term = Term.(const project_hook_create $ project_id $ url) in
  Cmd.v info term

let project_hooks_cmd config =
  let project_hooks project_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      Project.Hook.list ~token:config.token ~project_id () >|~ fun p ->
        printf "%s\n" (Yojson.Basic.prettify @@ Gitlab_j.string_of_project_hooks p)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List all project hooks." in
  let info = Cmd.info ~envs ~doc "list" in
  let term = Term.(const project_hooks $ project_id)  in
  Cmd.v info term

let project_hook_cmd config =
  let project_hook project_id hook_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      Project.Hook.by_id ~token:config.token ~project_id ~hook_id () >|~ fun p ->
        printf "%s\n" (Yojson.Basic.prettify @@ Gitlab_j.string_of_project_hook p)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Display project hook detail." in
  let info = Cmd.info ~envs ~doc "info" in
  let term = Term.(const project_hook $ project_id $ hook_id)  in
  Cmd.v info term

let cmd config =
  let doc = "Create, view and list project hooks." in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man = [ ] in
  let info = Cmd.info ~envs "ph" ~doc ~man in
  Cmd.group ~default info
    [ project_hook_create_cmd config;
      project_hooks_cmd config;
      project_hook_cmd config;
    ]
