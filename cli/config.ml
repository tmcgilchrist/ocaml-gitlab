type config = { token : Gitlab.Token.t; user : string }

exception Config of string

let from_file () =
  let home = Unix.getenv "HOME" in
  let fpath = home ^ "/.config/lab" in
  let result = Otoml.Parser.from_file_result fpath in
  match result with
  | Ok toml ->
      let user = Otoml.(find toml get_string [ "gitlab.com"; "user" ])
      and token' = Otoml.(find toml get_string [ "gitlab.com"; "token" ]) in
      { token = Gitlab.Token.of_string token'; user }
  | Error err -> raise (Config err)
