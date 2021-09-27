module Time = struct
  let now = Unix.gettimeofday

  let sleep = Lwt_unix.sleep
end

module Env = struct
  let debug = try Unix.getenv "GITLAB_DEBUG" <> "0" with _ -> false

  let gitlab_uri =
    try Unix.getenv "GITLAB_URL" with _ -> "https://gitlab.com/api/v4"
end

include Gitlab_core.Make (Env) (Time) (Cohttp_lwt_unix.Client)
