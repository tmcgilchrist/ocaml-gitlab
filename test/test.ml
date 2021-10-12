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

let check_diff dir s =
  let expected = dir / "expected.json" in
  let output = dir / "output.json" in
  let oc = open_out output in
  output_string oc s;
  close_out oc;
  (* TODO Do type comparison rather than via strings and external jd tool. *)
  let diff = Printf.sprintf "jd -set %s %s" expected output in
  let diff_out, diff_in = Unix.open_process diff in
  let diff_output = read_ic diff_out in
  match Unix.close_process (diff_out, diff_in), String.length diff_output with
  | Unix.WEXITED 0, 0 -> ()
  | Unix.WEXITED x, _ ->
      Alcotest.fail ("diff failed " ^ string_of_int x ^ ":\n" ^ diff_output)
  | _, _ -> Alcotest.fail ("diff failed :\n" ^ diff_output)

module type TestableJson = sig
  type t

  val name : string (* directory for serialisation example *)

  val of_string : string -> t

  val pp : t -> string
end

module Make (M : TestableJson) = struct
  let test () =
    let open Alcotest_lwt in
    let dir = "cases" / M.name in
    let str = read_file (dir / "event.json") in
    let a =
      try M.of_string str
      with e ->
        Alcotest.fail (M.name ^ " failed with: " ^ Printexc.to_string e)
    in
    [ test_case_sync M.name `Quick (fun () -> check_diff dir (M.pp a)) ]
end

module Gitlab_j_events : TestableJson = struct
  type t = Gitlab_j.events

  let name = "events"

  let of_string = Gitlab_j.events_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_events v)
end

module Gitlab_j_user_short : TestableJson = struct
  type t = Gitlab_j.user_short

  let name = "user_short"

  let of_string = Gitlab_j.user_short_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_user_short v)
end

module Gitlab_j_projects : TestableJson = struct
  type t = Gitlab_j.projects_full

  let name = "projects"

  let of_string = Gitlab_j.projects_full_of_string

  let pp v =
    Yojson.Basic.prettify (Gitlab_j.string_of_projects_full v)
end

module Gitlab_j_project_short : TestableJson = struct
  type t = Gitlab_j.project_short

  let name = "project_short"

  let of_string = Gitlab_j.project_short_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_project_short v)
end

module Gitlab_j_webhooks : TestableJson = struct
  type t = Gitlab_j.webhooks

  let name = "webhooks"

  let of_string = Gitlab_j.webhooks_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_webhooks v)
end

module Gitlab_j_merge_requests : TestableJson = struct
  type t = Gitlab_j.merge_requests

  let name = "merge_requests"

  let of_string = Gitlab_j.merge_requests_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_merge_requests v)
end

module Gitlab_j_commit_statuses : TestableJson = struct
  type t = Gitlab_j.commit_statuses

  let name = "commit_statuses"

  let of_string = Gitlab_j.commit_statuses_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_commit_statuses v)
end


(* instances under test *)
module E = Make (Gitlab_j_events)
module US = Make (Gitlab_j_user_short)
module P = Make (Gitlab_j_projects)
module PS = Make (Gitlab_j_project_short)
module WH = Make (Gitlab_j_webhooks)
module MR = Make (Gitlab_j_merge_requests)
module CS = Make (Gitlab_j_commit_statuses)

(* Run it *)
let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "GitLab"
       [
         ("commit_statuses", CS.test ());
         ("events", E.test ());
         ("merge_requests", MR.test ());
         ("project_short", PS.test ());
         ("projects", P.test ());
         ("user_short", US.test ());
         ("webhooks", WH.test ());
       ]
