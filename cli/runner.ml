open Cmdliner
open Config

let envs = Gitlab.Env.envs

let json =
  let doc = "Print output as formatted json" in
  Arg.(value & flag & info [ "json" ] ~doc)

let list_cmd config =
  let pp f r =
    Fmt.pf f "%-20i %-20s" r.Gitlab_j.runner_id r.Gitlab_j.runner_description
  in
  let printer runners json =
    if json then
      Fmt.pr "%s\n" (Yojson.Basic.prettify @@ Gitlab_j.string_of_runners runners)
    else
      Fmt.pr "%-20s %-20s\n%s" "runner_id" "description"
        (Fmt.str "%a\n" (Fmt.list ~sep:(Fmt.any "\n") pp) runners)
  in
  let list json =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      Runners.list ~token:config.token () >|~ fun runners -> printer runners json
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List all runners available to the user." in
  let info = Cmd.info ~envs ~doc "list" in
  let term = Term.(const list $ json) in
  Cmd.v info term

let group_name = "runner"

let cmd config =
  let doc = "Manage runners." in
  let default = Term.(ret (const (`Help (`Pager, Some group_name)))) in
  let man = [] in
  let info = Cmd.info ~envs group_name ~doc ~man in
  Cmd.group ~default info [ list_cmd config ]
