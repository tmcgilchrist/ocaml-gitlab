open Cmdliner
open Printf
open Config

let commit_sha =
  let doc = "A commit SHA or branch name (default: \"HEAD\")." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"COMMIT" ~doc)

let project_id =
  let doc = "Project Id" in
    Arg.(
    required
      & opt (some int) None
      & info [ "p"; "project-id" ] ~docv:"PROJECT_ID" ~doc)

let verbose =
 let doc = "Print detailed report of all status checks and their URLs." in
 Arg.(value & opt bool false & info [ "v"; "verbose" ] ~doc)

let state =
  let doc = "CI State" in
  let commit_status_status =
    Arg.enum
      [
        ("pending", `Pending);
        ("running", `Running);
        ("success", `Success);
        ("failed", `Failed);
        ("cancelled", `Cancelled);
      ]
  in
    Arg.(
    required
      & pos 1 (some commit_status_status) (Some `Pending)
      & info [] ~docv:"STATE" ~doc)

let envs = Gitlab.Env.envs

let merge_requests_cmd config =
  let merge_requests_list () =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let* mr = return (User.merge_requests ~token:config.token ()) in
      Stream.iter
        (fun merge_request ->
          printf "#%i %s\n" merge_request.Gitlab_t.merge_request_id
            merge_request.Gitlab_t.merge_request_title;
          return ())
        mr
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List user's merge requests." in
  let info = Cmd.info ~envs ~doc "merge-requests" in
  let term = Term.(const merge_requests_list $ const ()) in
  Cmd.v info term


let ci_status_cmd config =
  let ci_status project_id sha _verbose =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      return @@ Project.Commit.statuses ~token:config.token ~project_id ~sha ()
      >>= fun statuses ->
      let* results = Stream.to_list statuses in
      return
      @@
      match List.length results > 0 with
      | true ->
          List.iter
            (fun status -> printf "%s\n" status.Gitlab_t.commit_status_status)
            results
      | false -> printf "failure\n"
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List build status of a commit." in
  let info = Cmd.info ~envs ~doc "ci-status" in
  let term = Term.(const ci_status $ project_id $ commit_sha $ verbose) in
  Cmd.v info term

let ci_status_set_cmd config =
  let ci_status project_id sha state =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let new_status =
        {
          Gitlab_t.state;
          ref_name = None;
          name = None;
          target_url = None;
          pipeline_id = None;
          coverage = None;
          description = None;
        }
      in
      Project.Commit.status ~token:config.token ~project_id ~sha new_status ()
      >|~ fun status ->
      printf "%s\n"
        (Yojson.Basic.prettify (Gitlab_j.string_of_commit_status status))
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Set or update the build status of a commit." in
  let info = Cmd.info ~envs ~doc "set-ci-status" in
  let term = Term.(const ci_status $ project_id $ commit_sha $ state) in
  Cmd.v info term

let cmd config =
  let doc = "Work with GitLab merge requests." in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man = [ ] in
  let info = Cmd.info ~envs "mr" ~doc ~man in
  Cmd.group ~default info
    [ merge_requests_cmd config;
      ci_status_cmd config;
      ci_status_set_cmd config;
    ]
