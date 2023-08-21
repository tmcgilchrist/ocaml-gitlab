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

let yojson =
  let module M = struct
    type t = Yojson.Basic.t

    let pp f t = Fmt.pf f "%s" (Yojson.Basic.pretty_to_string t)
    let equal = Yojson.Basic.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

module type TestableJson = sig
  type t

  val name : string (* directory for serialisation example *)
  val of_string : string -> t
  val to_json : t -> Yojson.Basic.t
end

module Make (M : TestableJson) = struct
  let test () =
    let open Alcotest in
    let base = "cases" / M.name in
    let test_file = read_file (base / "event.json") in
    let diff_json output =
      let expected =
        M.to_json @@ M.of_string @@ read_file (base / "expected.json")
      in
      Alcotest.(check yojson) "diff-json" expected output
    in
    let a =
      try M.of_string test_file
      with e ->
        Alcotest.fail (M.name ^ " failed with: " ^ Printexc.to_string e)
    in
    [ test_case M.name `Quick (fun () -> diff_json (M.to_json a)) ]
end

module Gitlab_j_events : TestableJson = struct
  type t = Gitlab_j.events

  let name = "events"
  let of_string = Gitlab_j.events_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_events v)
end

module Gitlab_j_user_short : TestableJson = struct
  type t = Gitlab_j.user_short

  let name = "user_short"
  let of_string = Gitlab_j.user_short_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_user_short v)
end

module Gitlab_j_user : TestableJson = struct
  type t = Gitlab_j.user

  let name = "user"
  let of_string = Gitlab_j.user_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_user v)
end

module Gitlab_j_current_user : TestableJson = struct
  type t = Gitlab_j.current_user

  let name = "current_user"
  let of_string = Gitlab_j.current_user_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_current_user v)
end

module Gitlab_j_projects : TestableJson = struct
  type t = Gitlab_j.projects_full

  let name = "projects"
  let of_string = Gitlab_j.projects_full_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_projects_full v)
end

module Gitlab_j_project_short : TestableJson = struct
  type t = Gitlab_j.project_short

  let name = "project_short"
  let of_string = Gitlab_j.project_short_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_project_short v)
end

module Gitlab_j_project_hook : TestableJson = struct
  type t = Gitlab_j.project_hook

  let name = "project_hook"
  let of_string = Gitlab_j.project_hook_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_project_hook v)
end

module Gitlab_j_webhooks : TestableJson = struct
  type t = Gitlab_j.webhooks

  let name = "webhooks"
  let of_string = Gitlab_j.webhooks_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_webhooks v)
end

module Gitlab_j_merge_requests : TestableJson = struct
  type t = Gitlab_j.merge_requests

  let name = "merge_requests"
  let of_string = Gitlab_j.merge_requests_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_merge_requests v)
end

module Gitlab_j_notes : TestableJson = struct
  type t = Gitlab_j.notes

  let name = "notes"
  let of_string = Gitlab_j.notes_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_notes v)
end

module Gitlab_j_commit_statuses : TestableJson = struct
  type t = Gitlab_j.commit_statuses

  let name = "commit_statuses"
  let of_string = Gitlab_j.commit_statuses_of_string

  let to_json v =
    Yojson.Basic.from_string (Gitlab_j.string_of_commit_statuses v)
end

module Gitlab_j_commits : TestableJson = struct
  type t = Gitlab_j.commits

  let name = "commits"
  let of_string = Gitlab_j.commits_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_commits v)
end

module Gitlab_j_branches_full : TestableJson = struct
  type t = Gitlab_j.branches_full

  let name = "branches"
  let of_string = Gitlab_j.branches_full_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_branches_full v)
end

module Gitlab_j_milestones : TestableJson = struct
  type t = Gitlab_j.milestones

  let name = "milestones"
  let of_string = Gitlab_j.milestones_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_milestones v)
end

module Gitlab_j_issues : TestableJson = struct
  type t = Gitlab_j.issues

  let name = "issues"
  let of_string = Gitlab_j.issues_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_issues v)
end

module Gitlab_j_runners : TestableJson = struct
  type t = Gitlab_j.runners

  let name = "runners"
  let of_string = Gitlab_j.runners_of_string
  let to_json v = Yojson.Basic.from_string (Gitlab_j.string_of_runners v)
end

(* instances under test *)
module E = Make (Gitlab_j_events)
module CU = Make (Gitlab_j_current_user)
module U = Make (Gitlab_j_user)
module US = Make (Gitlab_j_user_short)
module P = Make (Gitlab_j_projects)
module PS = Make (Gitlab_j_project_short)
module PH = Make (Gitlab_j_project_hook)
module WH = Make (Gitlab_j_webhooks)
module MR = Make (Gitlab_j_merge_requests)
module N = Make (Gitlab_j_notes)
module CS = Make (Gitlab_j_commit_statuses)
module BF = Make (Gitlab_j_branches_full)
module M = Make (Gitlab_j_milestones)
module C = Make (Gitlab_j_commits)
module I = Make (Gitlab_j_issues)
module R = Make (Gitlab_j_runners)

module Stream_test = struct
  let run m = Lwt_main.run (Gitlab.Monad.run m)

  let test_take =
    Alcotest.test_case "take" `Quick (fun () ->
        List.iter
          (fun (input, n, expected) ->
            Alcotest.(check (list int))
              "stream-take" expected
              (run Gitlab.Stream.(input |> of_list |> take n |> to_list)))
          [
            ([], 0, []);
            ([ 1; 2; 3; 4; 5; 6 ], 0, []);
            ([ 1; 2; 3; 4; 5; 6 ], 10, [ 1; 2; 3; 4; 5; 6 ]);
            ([ 1; 2; 3; 4; 5; 6 ], 1, [ 1 ]);
          ])

  let test_map =
    Alcotest.test_case "map" `Quick (fun () ->
        let input = [ 1; 2; 3; 4 ] in
        Alcotest.(check (list int))
          "stream-map" [ 1; 1; 2; 2; 3; 3; 4; 4 ]
          (run
             Gitlab.Stream.(
               input |> of_list
               |> map (fun x -> Gitlab.Monad.return [ x; x ])
               |> to_list)))

  let test_map_take =
    Alcotest.test_case "map-take" `Quick (fun () ->
        let input = [ 1; 2; 3; 4 ] in
        Alcotest.(check (list int))
          "stream-take-map" [ -1; -2 ]
          (run
             Gitlab.Stream.(
               input |> of_list |> take 2
               |> map (fun i -> Gitlab.Monad.return [ -i ])
               |> to_list)))

  let tests = [ test_take; test_map; test_map_take ]
end

(* Run it *)
let () =
  let open Alcotest in
  run "GitLab"
    [
      ("branches", BF.test ());
      ("commit_statuses", CS.test ());
      ("commits", C.test ());
      ("events", E.test ());
      ("issues", I.test ());
      ("merge_requests", MR.test ());
      ("notes", N.test ());
      ("milestones", M.test ());
      ("project_short", PS.test ());
      ("projects", P.test ());
      ("runners", R.test ());
      ("current user", CU.test ());
      ("user", U.test ());
      ("user_short", US.test ());
      ("webhooks", WH.test ());
      ("project_hook", PH.test ());
      ("stream_test", Stream_test.tests);
    ]
