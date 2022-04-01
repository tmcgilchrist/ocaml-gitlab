let find_dot_git () =
  let rec search dir =
    let dot_git = Filename.concat dir ".git" in
    if Sys.file_exists dot_git && Sys.is_directory dot_git then
      Some dot_git
    else
      let parent_dir = Filename.dirname dir in
      if not (String.equal dir parent_dir) then
        search parent_dir
      else
        None
  in
  search (Sys.getcwd ())

let parse_dot_git () =
  let dot_git = find_dot_git () in
  match dot_git with
  | None -> Error "No .git directory found in this directory, nor in a parent \
              directory."
  | Some dot_git ->
    let config_file = Filename.concat dot_git "config" in
    begin try
      let chan = open_in config_file in
      let lexbuf = Lexing.from_channel chan in
      begin try
        Ok (Parser.config Lexer.token lexbuf)
      with
      | Lexer.Error msg ->
          Error (Printf.sprintf "Error in lexer while parsing %s: %s%!"
            config_file msg)
      | Parser.Error ->
          Error (Printf.sprintf
            "Error while parsing %s: At offset %d: syntax error.\n%!"
            config_file
            (Lexing.lexeme_start lexbuf))
      end
    with Sys_error e ->
      Error ("Error when opening " ^ config_file ^ ": " ^ e)
    end

let gitlab_project_from_url url =
  let regex = Str.regexp
    "^git@[A-Za-z0-9.-]+:\\([A-Za-z0-9-]+/[A-Za-z-]+\\)\\(\\.git\\)?$\
    \\|\
    ^https:[A-Za-z0-9./-]+/\\([A-Za-z0-9-]+/[A-Za-z-]+\\)\\(\\.git\\)?$"
  in
  if Str.string_match regex url 0 then Some (Str.matched_group 1 url) else None

let gitlab_project_name ?remote config =
  match remote with
  | Some remote ->
    begin try
      let settings = List.assoc ("remote \"" ^ remote ^ "\"") config in
      let url = List.assoc "url" settings in
      gitlab_project_from_url url
    with Not_found -> None
    end
  | None ->
    let remotes =
      List.filter
        (fun (name,_) ->
          let regex = Str.regexp "^remote \"[^\"]+\"$" in
          Str.string_match regex name 0)
        config
    in
    begin match remotes with
    | [(_name,settings)] ->
        Printf.printf "case 1\n";
        begin try
          gitlab_project_from_url (List.assoc "url" settings)
        with Not_found -> None
        end
    | _ :: _ | [] -> Printf.printf "case 2\n"; None
    end
