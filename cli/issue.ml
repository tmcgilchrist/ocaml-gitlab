open Cmdliner

let envs = Gitlab.Env.envs

let cmd _config = 
  let doc = "Manage issues." in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man = [ ] in
  let info = Cmd.info ~envs "issue" ~doc ~man in
  Cmd.group ~default info [ ]  

