let ( / ) = Filename.concat

let read_ic ic =
  let rec loop prev =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop (line :: prev)
    | None -> List.rev prev
  in
  String.concat "\n" (loop [])

let read_file path =
  let ic = open_in path in
  let contents = read_ic ic in
  close_in ic;
  contents

(* Enum of all tests in test/cases/* *)
type json_type =
  | Events
  | User_short
  | Webhooks
  | Merge_requests
  | Commit_statuses
  | Branches
  | Milestones

open Cmdliner

let json_type =
  let json_type_enum =
    Arg.enum
      [
        ("events", Events);
        ("user_short", User_short);
        ("webhooks", Webhooks);
        ("merge_requests", Merge_requests);
        ("commit_statuses", Commit_statuses);
        ("branches", Branches);
        ("milestones", Milestones);
      ]
  in
  Arg.(
    required
    & pos 0 (some json_type_enum) None
    & info [] ~docv:"JSON_TYPE" ~doc:"Json Type")

(* Testable module for abstracting over what to round-trip test. *)
module type Testable = sig
  type t

  val name : string

  val of_string : string -> t

  val pp : t -> string
end

module Gitlab_j_events : Testable = struct
  type t = Gitlab_j.events

  let name = "events"

  let of_string = Gitlab_j.events_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_events v
end

module Gitlab_j_user_short : Testable = struct
  type t = Gitlab_j.user_short

  let name = "user_short"

  let of_string = Gitlab_j.user_short_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_user_short v
end

module Gitlab_j_webhooks : Testable = struct
  type t = Gitlab_j.webhooks

  let name = "webhooks"

  let of_string = Gitlab_j.webhooks_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_webhooks v
end

module Gitlab_j_merge_requests : Testable = struct
  type t = Gitlab_j.merge_requests

  let name = "merge_requests"

  let of_string = Gitlab_j.merge_requests_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_merge_requests v
end

module Gitlab_j_commit_statuses : Testable = struct
  type t = Gitlab_j.commit_statuses

  let name = "commit_statuses"

  let of_string = Gitlab_j.commit_statuses_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_commit_statuses v
end

module Gitlab_j_branches : Testable = struct
  type t = Gitlab_j.branches_full

  let name = "branches"

  let of_string = Gitlab_j.branches_full_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_branches_full v
end

module Gitlab_j_milestones : Testable = struct
  type t = Gitlab_j.milestones

  let name = "milestones"

  let of_string = Gitlab_j.milestones_of_string

  let pp v =
    Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string
    @@ Gitlab_j.string_of_milestones v
end

(* Make a runnable roundtrip test of Testable.
   This is integrated with dune as a cli test and
   diff that lets us use --auto-promote to accept diffs
   in json output.
*)
module Make (M : Testable) = struct
  let run () =
    let dir = "cases" / M.name in
    let str = read_file (dir / "event.json") in
    let pp = M.pp @@ M.of_string str in
    Printf.printf "%s" pp
end

let cmd =
  let api json_type =
    match json_type with
    | Events ->
        let module E = Make (Gitlab_j_events) in
        E.run ()
    | User_short ->
        let module E = Make (Gitlab_j_user_short) in
        E.run ()
    | Webhooks ->
        let module E = Make (Gitlab_j_webhooks) in
        E.run ()
    | Merge_requests ->
        let module E = Make (Gitlab_j_merge_requests) in
        E.run ()
    | Commit_statuses ->
        let module E = Make (Gitlab_j_commit_statuses) in
        E.run ()
    | Branches ->
        let module E = Make (Gitlab_j_branches) in
        E.run ()
    | Milestones ->
        let module E = Make (Gitlab_j_milestones) in
        E.run ()
  in
  (Term.(pure api $ json_type), Term.info "diff")

let () = match Term.eval ~catch:true cmd with `Error _ -> exit 1 | _ -> exit 0
