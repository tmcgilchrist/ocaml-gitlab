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

let job_id =
  let doc = "Job Id" in
  Arg.(
    required & opt (some int) None & info [ "j"; "job-id" ] ~docv:"JOB_ID" ~doc)

let job_token =
  let doc = "Job Token" in
  Arg.(
    value
    & opt (some string) None
    & info [ "t"; "job-token" ] ~docv:"JOB_TOKEN" ~doc)

let ref_name =
  let doc =
    "Branch or tag name in repository. HEAD or SHA references are not supported"
  in
  Arg.(
    required
    & opt (some string) None
    & info [ "r"; "ref-name" ] ~docv:"REF_NAME" ~doc)

let job_name =
  let doc = "The name of the job" in
  Arg.(
    required
    & opt (some string) None
    & info [ "n"; "job-name" ] ~docv:"JOB_NAME" ~doc)

let artifact_path =
  let doc = "Artifact path" in
  Arg.(
    required
    & opt (some string) None
    & info [ "a"; "artifact-path" ] ~docv:"ARTIFACT_PATH" ~doc)

let output_file =
  let doc = "Output file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output-file" ] ~docv:"OUTPUT_FILE" ~doc)

let with_open_out file write_f =
  let chan = open_out file in
  try
    write_f chan;
    close_out chan
  with x ->
    close_out chan;
    raise x

let write_file filename ~contents =
  with_open_out filename @@ fun ch -> output_string ch contents

let write_response_to_file ~output_file response =
  match response with
  | None -> printf "Found no artifact\n"
  | Some artifact ->
      write_file output_file ~contents:artifact;
      printf "Wrote %s\n" output_file

let get_cmd config =
  let get job_token project_id job_id output_file =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let output_file =
        let default =
          Format.asprintf "job-artifacts-%d-%d.zip" project_id job_id
        in
        Option.value ~default output_file
      in
      Job_artifacts.get ~token:config.token ?job_token ~project_id ~job_id ()
      >|~ write_response_to_file ~output_file
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Download a job's artifacts as a zip-file" in
  let info = Cmd.info ~envs ~doc "get" in
  let term = Term.(const get $ job_token $ project_id $ job_id $ output_file) in
  Cmd.v info term

let get_archive_cmd config =
  let get_archive job_token project_id ref_name job_name output_file =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let output_file =
        let default =
          Format.asprintf "job-artifacts-%d-%s-%s.zip" project_id ref_name
            job_name
        in
        Option.value ~default output_file
      in
      Job_artifacts.get_archive ~token:config.token ?job_token ~project_id
        ~ref_name ~job:job_name ()
      >|~ write_response_to_file ~output_file
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Download a job's artifacts as a zip-file" in
  let info = Cmd.info ~envs ~doc "get_archive" in
  let term =
    Term.(
      const get_archive $ job_token $ project_id $ ref_name $ job_name
      $ output_file)
  in
  Cmd.v info term

let get_file_cmd config =
  let get_file job_token project_id job_id artifact_path output_file =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let output_file =
        let default = Filename.basename artifact_path in
        Option.value ~default output_file
      in
      Job_artifacts.get_file ~token:config.token ?job_token ~project_id ~job_id
        ~artifact_path ()
      >|~ write_response_to_file ~output_file
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Download a job's artifacts as a zip-file" in
  let info = Cmd.info ~envs ~doc "get_file" in
  let term =
    Term.(
      const get_file $ job_token $ project_id $ job_id $ artifact_path
      $ output_file)
  in
  Cmd.v info term

let cmd config =
  let doc = "Work with GitLab artifacts." in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man = [] in
  let info = Cmd.info ~envs "artifact" ~doc ~man in
  Cmd.group ~default info
    [ get_cmd config; get_archive_cmd config; get_file_cmd config ]
