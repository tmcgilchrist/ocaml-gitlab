open Cmdliner

let envs = Gitlab.Env.envs

let cmds =
  let doc = "Work seamlessly with GitLab from the command line." in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
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
        `S "FEEDBACK";
        `P "Open an issue using 'lab issue create -R github.com/tmcgilchrist/ocaml-gitlab' "
      ] in
  let info = Cmd.info ~envs "lab" ~version:"%%VERSION_NUM%%" ~doc ~man in
  let config = Config.from_file in
  Cmd.group ~default info
  [
    Api.cmd config;
    Issue.cmd config;
    Job_artifact.cmd config;
    Merge_request.cmd config;
    Project.cmd config;
    User.cmd config;
    Runner.cmd config;
    Project_hook.cmd config;
  ]

let () =
  exit @@ Cmd.eval ~catch:true cmds
