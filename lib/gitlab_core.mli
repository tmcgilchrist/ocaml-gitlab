(** Portable Functor to the GitLab API. *)

module Make
    (Env : Gitlab_s.Env)
    (Time : Gitlab_s.Time)
    (CL : Cohttp_lwt.S.Client) : Gitlab_s.Gitlab
