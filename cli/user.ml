open Cmdliner
open Printf
open Config

let envs = Gitlab.Env.envs

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

let json =
  let doc = "Print output as formatted json" in
  Arg.(value & flag & info [ "json" ] ~doc)

let user_cmd =
  let user_list user =
    let cmd =
      let open Gitlab in
      let open Monad in
      User.by_id ~id:user () >>~ fun user ->
      return @@ printf "%s\n" user.Gitlab_t.user_username
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Display user name and id." in
  let info = Cmd.info ~envs ~doc "info" in
  let term = Term.(const user_list $ owner_id) in
  Cmd.v info term

let user_name_cmd =
  let user_list name json =
    let cmd name =
      let open Gitlab in
      let open Monad in
      User.by_name ~name () >|~ fun users ->
      if json then
        printf "%s" (Yojson.Basic.prettify (Gitlab_j.string_of_users users))
      else
        List.iter
          (fun user ->
            printf "%s:%i\n" user.Gitlab_t.user_short_username
              user.Gitlab_t.user_short_id)
          users
    in
    Lwt_main.run @@ Gitlab.Monad.run (cmd name)
  in
  let doc = "Display users by name and id." in
  let info = Cmd.info ~envs ~doc "name" in
  let term = Term.(const user_list $ owner_name $ json) in
  Cmd.v info term

let user_projects_cmd =
  let user_projects_list id =
    let cmd =
      let open Gitlab in
      let open Monad in
      User.projects ~id () >|~ fun projects ->
      List.iter
        (fun project -> printf "%s\n" project.Gitlab_t.project_short_name)
        projects
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List public projects owned by the user." in
  let info = Cmd.info ~envs ~doc "projects"  in
  let term = Term.(const user_projects_list $ owner_id) in
  Cmd.v info term

let user_events_cmd config =
  let user_projects_list id =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      User.events ~token:config.token ~id () >|~ fun events ->
      printf "%s\n" (Yojson.Basic.prettify @@ Gitlab_j.string_of_events events)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List all user events." in
  let info = Cmd.info ~envs ~doc "events" in
  let term =  Term.(const user_projects_list $ owner_id) in
  Cmd.v info term

let group_name = "user"

let cmd config =
  let doc = "Work with GitLab users." in
  let default = Term.(ret (const (`Help (`Pager, Some group_name)))) in
  let man = [ ] in
  let info = Cmd.info ~envs group_name ~doc ~man in
  Cmd.group ~default info
    [ user_cmd;
      user_name_cmd;
      user_projects_cmd;
      user_events_cmd config;
    ]