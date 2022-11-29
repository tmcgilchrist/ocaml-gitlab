open Cmdliner
open Config

let envs = Gitlab.Env.envs

let api =
  let doc = "The GitLab API endpoint to send the HTTP request to." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ENDPOINT" ~doc)

let cmd config =
  let api_cmd uri_str =
    let cmd =
      let open Gitlab in
      let open Monad in
      let config = config () in
      let uri = Uri.of_string uri_str in
      API.get ~token:config.token ~uri (fun body ->
          Lwt.return (Yojson.Basic.from_string body))
      >>~ fun json -> embed @@ Lwt_io.printf "%s" (Yojson.Basic.pretty_to_string json)
    in
    Lwt_main.run @@ Gitlab.Monad.run cmd
  in
  let doc = "Make an authenticated GitLab API request." in
  let info = Cmd.info ~doc ~envs "api" in
  let term = Term.(const api_cmd $ api) in
  Cmd.v info term
