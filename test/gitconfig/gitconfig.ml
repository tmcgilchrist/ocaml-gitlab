let print_config fmt config =
  let open Format in
  let rec print_bindings fmt = function
    | [] -> ()
    | (key, value) :: rem ->
        fprintf fmt "\t%s = %s\n" key value;
        print_bindings fmt rem
  in
  config
  |> List.iter (fun (name, bindings) ->
         fprintf fmt "[%s]\n%a" name print_bindings bindings)

let () =
  let lexbuf =
    Lexing.from_string
      {|[core]
	repositoryformatversion = 0
	filemode = true
	bare = false
	logallrefupdates = true
[remote "origin"]
	url = git@github.com:tmcgilchrist/ocaml-gitlab
	fetch = +refs/heads/*:refs/remotes/origin/*
[branch "master"]
	remote = origin
	merge = refs/heads/master
[branch "parse_gitconfig"]
	remote = origin
	merge = refs/heads/parse_gitconfig
|}
  in
  try
    let config = Gitconfig.Parser.config Gitconfig.Lexer.token lexbuf in
    print_config Format.std_formatter config
  with
  | Gitconfig.Lexer.Error msg -> Printf.fprintf stderr "%s%!" msg
  | Gitconfig.Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf)
