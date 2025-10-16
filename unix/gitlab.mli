include Gitlab_s.Gitlab

module Env : sig
  val envs : Cmdliner.Cmd.Env.info list
  (** [envs] is a list of environment variables that influence the library's
      behaviour. *)
end
