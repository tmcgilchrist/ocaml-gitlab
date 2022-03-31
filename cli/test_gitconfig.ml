let print_config fmt config =
  let open Format in
  let rec print_bindings fmt = function
  | [] -> ()
  | (key, value) :: rem ->
      fprintf fmt "\t%s = %s\n" key value;
      print_bindings fmt rem
  in
  config |> List.iter (fun (name, bindings) ->
      fprintf fmt "[%s]\n%a" name print_bindings bindings
    )


let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let config = Gitconfig_parser.config Gitconfig_lexer.token lexbuf in
    print_config Format.std_formatter config
  with
  | Gitconfig_lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Gitconfig_parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
