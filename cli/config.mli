type t = { token : Gitlab.Token.t; user : string }

exception Config of string

val from_file : unit -> t