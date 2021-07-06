module Time = struct
  let now = Unix.gettimeofday
  let sleep = Lwt_unix.sleep
end

include Gitlab_core.Make(Time)(Cohttp_lwt_unix.Client)
