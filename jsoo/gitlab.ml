module Time = struct
  let now = Unix.gettimeofday
  let sleep = Js_of_ocaml_lwt.Lwt_js.sleep
end

module Env = struct
  let debug = false
  let gitlab_uri = "https://gitlab.com/api/v4"
end

include Gitlab_core.Make (Env) (Time) (Cohttp_lwt_jsoo.Client)
