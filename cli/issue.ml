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

let group_id =
  let doc = "Group Id" in
  Arg.(
    required
    & opt (some int) None
    & info [ "g"; "group-id" ] ~docv:"GROUP_ID" ~doc)

let issue_printer issue =
  Gitlab_j.(
    printf
      "#%i / %s\n\
       \t- project_id: %i\n\
       \t- state: %s\n\
       \t- label(s): %s\n\
       \t- url: %s\n"
      issue.issue_iid issue.issue_title issue.issue_project_id
      (string_of_state issue.issue_state)
      (if issue.issue_labels = [] then "<none>"
      else String.concat ", " issue.issue_labels)
      issue.issue_web_url);
  Gitlab.Monad.return ()

let user_issue_subcmd config =
  let issues_list () =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let* issues = return (User.issues ~token:config.token ()) in

      Stream.iter issue_printer issues
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List user's issues." in
  let info = Cmd.info ~envs ~doc "user" in
  let term = Term.(const issues_list $ const ()) in
  Cmd.v info term

let project_issue_subcmd config =
  let issues_list project_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let* issues =
        return (Project.Issue.list ~token:config.token ~project_id ())
      in
      Stream.iter issue_printer issues
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List project's issues." in
  let info = Cmd.info ~envs ~doc "project" in
  let term = Term.(const issues_list $ project_id) in
  Cmd.v info term

let group_issue_subcmd config =
  let issues_list group_id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let* issues =
        return (Group.Issue.issues ~token:config.token ~group_id ())
      in
      Stream.iter issue_printer issues
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List group's issues." in
  let info = Cmd.info ~envs ~doc "group" in
  let term = Term.(const issues_list $ group_id) in
  Cmd.v info term

let issue_list_cmd config =
  let doc = "List issues." in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man = [] in
  let info = Cmd.info ~envs "list" ~doc ~man in
  Cmd.group ~default info
    [
      user_issue_subcmd config;
      project_issue_subcmd config;
      group_issue_subcmd config;
    ]

let group_name = "issue"

let cmd config =
  let doc = "Manage issues." in
  let default = Term.(ret (const (`Help (`Pager, Some group_name)))) in
  let man = [] in
  let info = Cmd.info ~envs group_name ~doc ~man in
  Cmd.group ~default info [ issue_list_cmd config ]
