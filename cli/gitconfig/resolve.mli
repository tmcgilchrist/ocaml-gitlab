val find_dot_git : unit -> string option
(** Search for a [.git] directory from the current directory upwards. Return
    [None] if none could be found. *)

val parse_dot_git : unit -> (Types.config, string) result
(** Search for a [.git] directory from the current directory upwards, and parse
    it. Return [Error] with an error message if none could be found, or in case
    of parse error. *)

val gitlab_project_name : ?remote:string -> Types.config -> string option
(** [gitlab_project_name ~remote config] extracts the Gitlab project name from
    the URL of the remote named [remote] in [config]. If [remote] is not
    specified, and there is more than one remote, returns [None]. Also returns
    [None] if no suitable remote URL can be found. *)
