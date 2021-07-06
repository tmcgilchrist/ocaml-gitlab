(** Portable functor to the Gitlab API. *)

module Make(Time : Gitlab_s.Time)(CL : Cohttp_lwt.S.Client): Gitlab_s.Gitlab

