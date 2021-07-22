open Cmdliner
open Printf
open Config

module CommandLine = struct
  let owner_id =
    let doc = "Gitlab Owner Id" in
    Arg.(required
         & opt (some string) None
         & info ["o"; "owner"] ~docv:"OWNER" ~doc)

  let owner_name =
    let doc = "Gitlab Ownername" in
    Arg.(required
         & opt (some string) None
         & info ["n"; "owner-name"] ~docv:"OWNER_NAME" ~doc)
end


let user_cmd =
  let user_list user () =
    Lwt_main.run begin
        let open Gitlab in
        let open Monad in
        run (
            User.by_id ~id:user ()
            >>~ fun user -> printf "%s\n" user.Gitlab_t.username; return ()
          )
      end  
  in

  Term.(pure user_list $ CommandLine.owner_id $ pure ()),
  Term.info "user-list"

let user_name_cmd =
  let user_list name () =
    Lwt_main.run begin
        let open Gitlab in
        let open Monad in
        run (
            User.by_name ~name ()
            >>~ fun users -> 
                List.iter (fun (user : Gitlab_t.user_short) -> printf "%s\n" user.Gitlab_t.username) users;
                return ()
          )
      end  
  in

  Term.(pure user_list $ CommandLine.owner_name $ pure ()),
  Term.info "user-name"

let user_projects_cmd =
  let user_projects_list id () =
    Lwt_main.run begin
        let open Gitlab in
        let open Monad in
        run (
            User.projects ~id ()
            >>~ fun projects ->
                List.iter (fun (project : Gitlab_t.project_short) -> printf "%s\n" project.Gitlab_t.name) projects;
                return ()
          )
      end  
  in

  Term.(pure user_projects_list $ CommandLine.owner_id $ pure ()),
  Term.info "user-projects"

let merge_requests_cmd =
  let merge_requests_list () =
    Lwt_main.run begin
        let open Gitlab in
        let open Monad in
        run (
            (* 
              TODO Auth token setup is different to Github. 
              See https://docs.gitlab.com/14.0/ee/api/README.html#authentication
             *)
            return (User.merge_requests ~token:access_token ()) >>=
            Stream.iter (fun merge_request ->
                printf "#%i %s\n" merge_request.Gitlab_j.id merge_request.Gitlab_t.title;
                return ())
          )
      end  
  in

  Term.(pure merge_requests_list $ pure ()),
  Term.info "merge-requests"

let default_cmd =
  let doc = "make git easier with GitLab" in
  Term.(ret (pure (`Help (`Pager, None)))),
  let man = [
      `S "DESCRIPTION";
      `P "Lab is a tool that wraps git in order to extend it with extra functionality that makes it better when working with GitLab.";
      `S "BUGS";
      `P "<https://github.com/mirage/ocaml-gitlab/issues>";
      `S "AUTHORS";
      `P "<https://github.com/mirage/ocaml-gitlab>"
    ] in
  Term.info "lab" ~version:"0.1" ~doc ~man


let cmds = [user_cmd; user_name_cmd; user_projects_cmd; merge_requests_cmd]

let () =
  match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0

