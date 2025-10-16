(** Portable functor to the GitLab API. *)

module Make (_ : Gitlab_s.Env) (_ : Gitlab_s.Time) (_ : Cohttp_lwt.S.Client) :
  Gitlab_s.Gitlab
