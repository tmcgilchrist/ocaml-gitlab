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

type mr_error =
  | GitRepoMissing of string
  | GitParsingError of string

let pp_error f err = match err with
  | GitRepoMissing str -> Fmt.pf f "%s" str
  | GitParsingError str -> Fmt.pf f "%s" str

let get_git_project =
  let open Result in
  let ( let* ) = bind in

  let* dot = Gitconfig.Resolve.parse_dot_git () |> map_error (fun x -> GitParsingError x) in
  let git_repo = Gitconfig.Resolve.gitlab_project_name ~remote:"origin" dot
          |> Option.map (String.split_on_char '/')
          |> Option.value ~default:[] in
  match git_repo with
  | (owner::name::[]) ->
     ok (owner, name)
  | _ ->
     error (GitRepoMissing "No gitlab project name found in .git/config.")

exception NotGitlabProject of string

let get_project token owner name : Gitlab_j.project_short Gitlab.Monad.t =
  let open Gitlab in
  let open Monad in

  Project.by_name ~token ~owner ~name () >>~ fun projects ->
  try return @@ List.hd projects
  with Failure _ -> fail @@ NotGitlabProject (owner ^ " " ^ name)


let merge_requests_list_cmd config =

  (* Pretty print line for merge request. *)
  let pp f (mr : Gitlab_j.merge_request) =
    Fmt.pf f "#%-19i %-20s %-20s"
      mr.merge_request_id
      mr.merge_request_title
      mr.merge_request_source_branch
  in

  let merge_requests_list () =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let token = config.token in
      match get_git_project with
      | Ok (owner, name) ->
         let* project = get_project token owner name in
         let id = project.Gitlab_j.project_short_id in
         let* _ = Monad.embed (
             Lwt_io.printf "\nShowing open pull requests in %s\n\n" project.Gitlab_j.project_short_path_with_namespace) in
         Project.merge_requests ~state:`Opened ~token ~id ()
         |> Stream.iter (fun merge_request ->
             return @@ Fmt.pr "%a\n" pp merge_request)
      | Error err ->
         return @@ Fmt.epr "Error: %a" pp_error err
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "List user's merge requests." in
  let info = Cmd.info ~envs ~doc "list" in
  let term = Term.(const merge_requests_list $ const ()) in
  Cmd.v info term

let mr_status_cmd config =
  let pp f (mr : Gitlab_j.merge_request) =
    let pp_state = function
      | `Unchecked -> "unchecked"
      | `Checking -> "checking"
      | `CanBeMerged -> "can_be_merged"
      | `CannotBeMerged -> "cannot_be_merged"
      | `CannotBeMergedRecheck -> "cannot_be_merged_recheck"
      | `Preparing -> "preparing"
    in
    Fmt.pf f "#%-19i %-20s %-20s %-20s"
      mr.merge_request_id
      mr.merge_request_title
      mr.merge_request_source_branch
      (pp_state mr.merge_request_merge_status)
  in
  let mr_status () =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let token = config.token in
      match get_git_project with
      | Ok (owner, name) ->
        let* project = get_project token owner name in
        let id = project.Gitlab_j.project_short_id in
        Project.merge_requests ~state:`Opened ~token ~id ~with_merge_status_recheck:true ()
        |> Stream.iter (fun merge_request ->
          return @@ Fmt.pr "%a\n" pp merge_request)
      | Error err -> 
        return @@ Fmt.epr "Error: %a" pp_error err
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd 
  in
  let doc = "Show status of relevant merge requests" in
  let info = Cmd.info ~envs ~doc "status" in
  let term = Term.(const mr_status $ const ()) in
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

(* let mr_show_cmd config =  *)
(*   let show mr_id = *)
(*     Lwt_main.run *)
(*   in *)
(*   let doc = "Set or update the build status of a commit." in *)
(*   let info = Cmd.info ~envs ~doc "set-ci-status" in *)
(*   let term = Term.(const show $ mr_id) in *)
(*   Cmd.v info term *)

let group_name = "mr"

let cmd config =
  let doc = "Work with GitLab merge requests." in
  let default = Term.(ret (const (`Help (`Pager, Some group_name)))) in
  let man = [ ] in
  let info = Cmd.info ~envs group_name ~doc ~man in
  Cmd.group ~default info
    [ merge_requests_list_cmd config;
      ci_status_cmd config;
      ci_status_set_cmd config;
      mr_status_cmd config;
      (* mr_show_cmd config; *)
    ]
