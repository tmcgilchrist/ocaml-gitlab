open Cmdliner

val cmd : (unit -> Config.t) -> unit Cmd.t
