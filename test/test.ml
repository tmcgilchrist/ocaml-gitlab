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
  match Unix.close_process (diff_out, diff_in) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x ->
      Alcotest.fail ("diff failed " ^ string_of_int x ^ ":\n" ^ diff_output)
  | _ -> Alcotest.fail "diff failed unexpectedly"

module type TestableJson = sig
  type t

  val name : string (* directory for serialisation example *)

  val of_string : string -> t

  val pp : t -> string
end

module Make (M : TestableJson) : sig
  val test : unit -> unit Alcotest.test_case list
end = struct
  let test () =
    let open Alcotest in
    let dir = "cases" / M.name in
    let str = read_file (dir / "event.json") in
    let a =
      try M.of_string str
      with e ->
        Alcotest.fail (M.name ^ " failed with: " ^ Printexc.to_string e)
    in
    [ test_case M.name `Quick (fun () -> check_diff dir (M.pp a)) ]
end

module Gitlab_j_events : TestableJson = struct
  type t = Gitlab_j.events
  let name = "events"
  let of_string = Gitlab_j.events_of_string
  let pp v = Yojson.Basic.pretty_to_string @@ Yojson.Basic.from_string (Gitlab_j.string_of_events v)
end

module Gitlab_j_user_short : TestableJson = struct
  type t = Gitlab_j.user_short

  let name = "user_short"

  let of_string = Gitlab_j.user_short_of_string

  let pp v =
    Yojson.Basic.pretty_to_string
    @@ Yojson.Basic.from_string (Gitlab_j.string_of_user_short v)
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

(* instances under test *)
module E = Make(Gitlab_j_events)
module US = Make (Gitlab_j_user_short)
module PS = Make (Gitlab_j_project_short)
module WH = Make (Gitlab_j_webhooks)

let passing =
  QCheck.Test.make ~count:1000 ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l)

(* TODO Write QC generators for types. *)
(* let failing = *)
(*   QCheck.Test.make ~count:10 *)
(*     ~name:"fail_sort_id" *)
(*     QCheck.(list small_int) *)
(*     (fun l -> l = List.sort compare l) *)

(* Run it *)
let () =
  let open Alcotest in
  run "GitLab"
    [
      (* "quick-check",     List.map QCheck_alcotest.to_alcotest [ passing(\* ; failing *\)]; *)
      ("events", E.test ());
      ("user_short", US.test ());
      ("project_short", PS.test ());
      ("webhooks", WH.test ());
    ]
