module Time = struct
  let now = Unix.gettimeofday

  let sleep = Lwt_unix.sleep
end

module Env = struct
  let debug = try Unix.getenv "GITLAB_DEBUG" <> "0" with _ -> false

  let gitlab_uri =
    try Unix.getenv "GITLAB_URL" with _ -> "https://gitlab.com/api/v4"

  let envs =
    let open Cmdliner in
    let doc = "The GitLab instance to connect to." in
    let gitlab_url = Cmd.Env.info "GITLAB_URL" ~doc in
    let doc = "Enable debugging (anything that's not 0)." in
    let gitlab_debug = Cmd.Env.info "GITLAB_DEBUG" ~doc in
    [gitlab_debug; gitlab_url]
end

include Gitlab_core.Make (Env) (Time) (Cohttp_lwt_unix.Client)
