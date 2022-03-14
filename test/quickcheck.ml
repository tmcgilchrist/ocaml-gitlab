(* generate a few trees, just to check what they look like: *)
(* QCheck.Gen.generate ~n:20 tree_gen *)

(* QCheck generators for events. *)

module Generator = struct
  open QCheck

  type event_action_name = [%import: Gitlab_t.event_action_name]
  [@@deriving qcheck]

  type event_target_type = [%import: Gitlab_t.event_target_type]
  [@@deriving qcheck]

  type user_state = [%import: Gitlab_t.user_state] [@@deriving qcheck]

  let gen_sha =
    let open Gen in
    let gen = oneof [ char_range '0' '9'; char_range 'a' 'f' ] in
    string_size (return 40) ~gen

  let gen_name = Gen.(oneofl Corpus.muppets)

  let gen_colour = Gen.(oneofl Corpus.colours)

  let gen_email =
    let open Gen in
    let* domain = oneofl Corpus.simpsons
    and* suffix = oneofl [ "net"; "com"; "org" ]
    and* user = gen_name in
    return (Printf.sprintf "%s@%s.%s" user domain suffix)

  let gen_url =
    let open Gen in
    let* domain = oneofl Corpus.simpsons
    and* suffix = oneofl [ "net"; "com"; "org"; "io" ]
    and* sha = gen_sha in
    return (Printf.sprintf "https://%s.%s/%s" domain suffix sha)

  let gen_git_ssh_url user project =
    Gen.return (Printf.sprintf "git@gitlab.com:%s/%s.git" user project)

  let gen_git_http_url user project =
    Gen.return (Printf.sprintf "https://gitlab.com/%s/%s.git" user project)

  let gen_namespace user project =
    Gen.return (Printf.sprintf "%s/%s" user project)

  let gen_homepage user project =
    Gen.return (Printf.sprintf "https://gitlab.com/%s/%s" user project)

  let gen_default_branch = Gen.oneofl [ "master"; "main" ]

  let gen_project =
    let open Gen in
    let* user = gen_name and* project = oneofl Corpus.animals in

    let* project_webhook_id = pint
    and* project_webhook_description = opt (oneofl Corpus.agile)
    and* project_webhook_web_url = Gen.string_printable
    and* project_webhook_avatar_url = opt Gen.string_printable
    and* project_webhook_ci_config_path = opt Gen.string_printable
    and* project_webhook_git_ssh_url = gen_git_ssh_url user project
    and* project_webhook_git_http_url = gen_git_http_url user project
    and* project_webhook_namespace = oneofl Corpus.boats
    and* project_webhook_path_with_namespace = gen_namespace user project
    and* project_webhook_visibility_level = pint
    and* project_webhook_default_branch = gen_default_branch
    and* project_webhook_homepage = opt (gen_homepage user project) in

    let* project_webhook_ssh_url = opt (return project_webhook_git_ssh_url)
    and* project_webhook_http_url = opt (return project_webhook_git_http_url) in
    return
      {
        Gitlab_t.project_webhook_id;
        project_webhook_name = project;
        project_webhook_description;
        project_webhook_web_url;
        project_webhook_avatar_url;
        project_webhook_ci_config_path;
        project_webhook_git_ssh_url;
        project_webhook_git_http_url;
        project_webhook_namespace;
        project_webhook_path_with_namespace;
        project_webhook_visibility_level;
        project_webhook_default_branch;
        project_webhook_homepage;
        project_webhook_url = project_webhook_homepage;
        project_webhook_ssh_url;
        project_webhook_http_url;
      }

  let gen_commit_url =
    Gen.return
      "https://gitlab.com/tmcgilchrist/freer/-/commit/f332bc71b0407c19e416c5813cc9334ebf65826e"

  let gen_timestamp = Gen.return "2016-04-17T02:36:16+00:00"

  let gen_daytime_utc =
    let open Gen in
    let* year = int_range 1998 2200 in
    let* month = int_range 1 12 in
    let* day = int_range 1 29 in
    return (Printf.sprintf "%i-%i-%i 04:17:18 UTC" year month day)

  let gen_author =
    let open Gen in
    let* author_name = gen_name and* author_email = gen_email in
    return { Gitlab_t.author_name; author_email }

  let gen_commit_webhook =
    let open Gen in
    let* commit_webhook_id = gen_sha
    and* commit_webhook_message = oneofl Corpus.nhl
    and* commit_webhook_title = oneofl Corpus.nfl
    and* commit_webhook_timestamp = gen_timestamp
    and* commit_webhook_url = gen_commit_url
    and* commit_webhook_author = gen_author
    and* commit_webhook_added = list (oneofl Corpus.fruits)
    and* commit_webhook_modified = list (oneofl Corpus.cats)
    and* commit_webhook_removed = list (oneofl Corpus.dogs) in
    return
      {
        Gitlab_t.commit_webhook_id;
        commit_webhook_message;
        commit_webhook_title;
        commit_webhook_timestamp;
        commit_webhook_url;
        commit_webhook_author;
        commit_webhook_added : string list;
        commit_webhook_modified : string list;
        commit_webhook_removed : string list;
      }

  let gen_repository : Gitlab_t.repository Gen.t =
    let open Gen in
    let* repository_name = gen_name and* author = gen_name in
    let* repository_url = gen_git_ssh_url author repository_name
    and* repository_description = list (oneofl Corpus.cats)
    and* repository_homepage = gen_homepage author repository_name
    and* repository_git_http_url = opt (gen_git_http_url author repository_name)
    and* repository_git_ssh_url = opt (gen_git_ssh_url author repository_name)
    and* repository_visibility_level = opt pint in
    return
      {
        Gitlab_t.repository_name;
        repository_url;
        repository_description = String.concat " " repository_description;
        repository_homepage;
        repository_git_http_url;
        repository_git_ssh_url;
        repository_visibility_level;
      }

  let gen_push_webhook : Gitlab_t.push_webhook Gen.t =
    let open Gen in
    let* push_webhook_event_name = oneofl Corpus.muppets
    and* push_webhook_before = gen_sha
    and* push_webhook_after = gen_sha
    and* push_webhook_ref = return "refs/heads/master"
    and* push_webhook_checkout_sha = gen_sha
    and* push_webhook_message = opt (oneofl Corpus.agile)
    and* push_webhook_user_id = pint
    and* push_webhook_user_name = gen_name
    and* push_webhook_user_email = gen_email
    and* push_webhook_user_avatar = oneofl Corpus.colours
    and* push_webhook_project_id = pint
    and* push_webhook_project = gen_project
    and* push_webhook_commits = list gen_commit_webhook
    and* push_webhook_repository = gen_repository in
    return
      {
        Gitlab_t.push_webhook_event_name;
        push_webhook_before;
        push_webhook_after;
        push_webhook_ref;
        push_webhook_checkout_sha;
        push_webhook_message;
        push_webhook_user_id;
        push_webhook_user_name;
        push_webhook_user_username = push_webhook_user_name;
        push_webhook_user_email;
        push_webhook_user_avatar;
        push_webhook_project_id;
        push_webhook_project;
        push_webhook_commits;
        push_webhook_total_commits_count = List.length push_webhook_commits;
        push_webhook_repository;
      }

  let gen_user_short : Gitlab_t.user_short Gen.t =
    let open Gen in
    let* user_short_id = pint
    and* user_short_name = gen_name
    and* user_short_state = opt gen_user_state
    and* user_short_avatar_url = gen_url
    and* user_short_web_url = opt gen_url
    and* user_short_email = opt gen_email in
    return
      {
        Gitlab_t.user_short_id;
        user_short_name;
        user_short_username = user_short_name;
        user_short_state;
        user_short_avatar_url;
        user_short_web_url;
        user_short_email;
      }

  let gen_label : Gitlab_t.label Gen.t =
    let open Gen in
    let* label_id = pint
    and* label_title = string_printable
    and* label_colour = gen_colour
    and* label_project_id = pint
    and* timestamp = gen_timestamp
    and* label_template = bool
    and* label_description = string_printable
    and* label_label_type = string_printable
    and* label_group_id = pint in
    return
      {
        Gitlab_t.label_id;
        label_title;
        label_colour;
        label_project_id;
        label_created_at = timestamp;
        label_updated_at = timestamp;
        label_template;
        label_description : string;
        label_label_type : string;
        label_group_id : int;
      }

  let gen_updated_by_id =
    let open Gen in
    let* update_by_id_current = pint in
    let* update_by_id_previous =
      opt (pint >>= fun x -> return (update_by_id_current + x))
    in
    return { Gitlab_t.update_by_id_previous; update_by_id_current }

  let gen_assignees : Gitlab_t.assignees Gen.t =
    let open Gen in
    let* assignees_previous = list gen_user_short in
    let* assignees_current = list gen_user_short in
    return { Gitlab_t.assignees_previous; assignees_current }

  let gen_changes : Gitlab_t.merge_request_changes Gen.t =
    let open Gen in
    let* merge_request_changes_updated_by_id = opt gen_updated_by_id
    and* merge_request_changes_updated_at =
      opt
        (map2
           (fun x y ->
             { Gitlab_t.updated_at_previous = x; updated_at_current = y })
           gen_daytime_utc gen_daytime_utc)
    and* merge_request_changes_assignees = opt gen_assignees in
    return
      {
        Gitlab_t.merge_request_changes_updated_by_id;
        merge_request_changes_updated_at;
        merge_request_changes_assignees;
      }

  let gen_commit_short_webhook : Gitlab_t.commit_short_webhook Gen.t =
    let open Gen in
    let* commit_short_webhook_id = string_printable
    and* commit_short_webhook_message = string_printable
    and* commit_short_webhook_title = string_printable
    and* commit_short_webhook_timestamp = gen_timestamp
    and* commit_short_webhook_url = gen_url
    and* commit_short_webhook_author = gen_author in
    return
      {
        Gitlab_t.commit_short_webhook_id;
        commit_short_webhook_message;
        commit_short_webhook_title;
        commit_short_webhook_timestamp;
        commit_short_webhook_url;
        commit_short_webhook_author;
      }

  let gen_merge_params : Gitlab_t.merge_params Gen.t =
    let open Gen in
    let* merge_params_force_remove_source_branch = string_printable in
    return
      {
        Gitlab_t.merge_params_force_remove_source_branch;
      }

  type merge_status = [%import: Gitlab_t.merge_status] [@@deriving qcheck]

  let gen_merge_request_attributes : Gitlab_t.merge_request_attributes Gen.t =
    let open Gen in
    let* merge_request_attributes_action = opt string_printable
    and* merge_request_attributes_assignee_id = opt pint
    and* merge_request_attributes_assignee_ids = list pint
    and* merge_request_attributes_author_id = pint
    and* merge_request_attributes_created_at = string_printable
    and* merge_request_attributes_description = string_printable
    and* merge_request_attributes_head_pipeline_id = opt pint
    and* merge_request_attributes_id = pint
    and* merge_request_attributes_iid = pint
    and* merge_request_attributes_last_edited_at = opt string_printable
    and* merge_request_attributes_last_edited_by_id = opt string_printable
    and* merge_request_attributes_last_commit = gen_commit_short_webhook
    and* merge_request_attributes_oldrev = opt string_printable
    and* merge_request_attributes_merge_commit_sha = opt string_printable
    and* merge_request_attributes_merge_error = opt string_printable
    and* merge_request_attributes_merge_params = gen_merge_params
    and* merge_request_attributes_merge_status = gen_merge_status
    and* merge_request_attributes_merge_user_id = opt pint
    and* merge_request_attributes_merge_when_pipeline_succeeds = bool
    and* merge_request_attributes_milestone_id = opt string_printable
    and* merge_request_attributes_source = gen_project
    and* merge_request_attributes_source_branch = string_printable
    and* merge_request_attributes_source_project_id = pint
    and* merge_request_attributes_state_id = pint
    and* merge_request_attributes_state = string_printable
    and* merge_request_attributes_target = gen_project
    and* merge_request_attributes_target_branch = string_printable
    and* merge_request_attributes_target_project_id = pint
    and* merge_request_attributes_title = string_printable
    and* merge_request_attributes_updated_at = string_printable
    and* merge_request_attributes_updated_by_id = opt pint
    and* merge_request_attributes_url = string_printable
    and* merge_request_attributes_work_in_progress = bool
    and* merge_request_attributes_total_time_spent = pint
    and* merge_request_attributes_time_change = pint
    and* merge_request_attributes_time_estimate = pint
    and* merge_request_attributes_human_total_time_spent = opt pint
    and* merge_request_attributes_human_time_change = opt pint
    and* merge_request_attributes_human_time_estimate = opt pint in
    return
      {
        Gitlab_t.merge_request_attributes_action;
        merge_request_attributes_assignee_id;
        merge_request_attributes_assignee_ids;
        merge_request_attributes_author_id;
        merge_request_attributes_created_at;
        merge_request_attributes_description;
        merge_request_attributes_head_pipeline_id;
        merge_request_attributes_id;
        merge_request_attributes_iid;
        merge_request_attributes_last_edited_at;
        merge_request_attributes_last_edited_by_id;
        merge_request_attributes_last_commit;
        merge_request_attributes_oldrev;
        merge_request_attributes_merge_commit_sha;
        merge_request_attributes_merge_error;
        merge_request_attributes_merge_params;
        merge_request_attributes_merge_status;
        merge_request_attributes_merge_user_id;
        merge_request_attributes_merge_when_pipeline_succeeds;
        merge_request_attributes_milestone_id;
        merge_request_attributes_source;
        merge_request_attributes_source_branch;
        merge_request_attributes_source_project_id;
        merge_request_attributes_state_id;
        merge_request_attributes_state;
        merge_request_attributes_target;
        merge_request_attributes_target_branch;
        merge_request_attributes_target_project_id;
        merge_request_attributes_title;
        merge_request_attributes_updated_at;
        merge_request_attributes_updated_by_id;
        merge_request_attributes_url;
        merge_request_attributes_work_in_progress;
        merge_request_attributes_total_time_spent;
        merge_request_attributes_time_change;
        merge_request_attributes_time_estimate;
        merge_request_attributes_human_total_time_spent;
        merge_request_attributes_human_time_change;
        merge_request_attributes_human_time_estimate;
     }

  let gen_merge_request_webhook : Gitlab_t.merge_request_webhook Gen.t =
    let open Gen in
    let* merge_request_webhook_user = gen_user_short
    and* merge_request_webhook_project = gen_project
    and* merge_request_webhook_repository = gen_repository
    and* merge_request_webhook_labels = list gen_label
    and* merge_request_webhook_attributes = gen_merge_request_attributes
    and* merge_request_webhook_changes = opt gen_changes
    and* merge_request_webhook_assignees = opt (list gen_user_short)
    in
    return
      {
        Gitlab_t.merge_request_webhook_event_type = "merge_request";
        merge_request_webhook_user;
        merge_request_webhook_project;
        merge_request_webhook_attributes : Gitlab_t.merge_request_attributes;
        merge_request_webhook_repository;
        merge_request_webhook_labels;
        merge_request_webhook_changes;
        merge_request_webhook_assignees;
      }

  let gen_webhook : Gitlab_t.webhook Gen.t =
    let open Gen in
    let push = map (fun x -> `Push x) gen_push_webhook in
    let merge_request =
      map (fun x -> `MergeRequest x) gen_merge_request_webhook
    in
    oneof [ push; merge_request ]

  let gen_webhooks : Gitlab_t.webhooks Gen.t =
    let open Gen in
    list gen_webhook

  let gen_user_short : Gitlab_t.user_short Gen.t =
    let open Gen in
    let* user_short_id = pint
    and* user_short_name = gen_name
    and* user_short_state = opt gen_user_state
    and* user_short_avatar_url = oneofl Corpus.cats
    and* user_short_web_url = opt (oneofl Corpus.fruits)
    and* user_short_email = opt (oneofl Corpus.vegetables) in
    return
      {
        Gitlab_t.user_short_id;
        user_short_name;
        user_short_username = user_short_name;
        user_short_state;
        user_short_avatar_url;
        user_short_web_url;
        user_short_email;
      }

  let gen_event : Gitlab_t.event Gen.t =
    let open Gen in
    let* event_id = pint in
    let* event_project_id = pint
    and* event_action_name = opt gen_event_action_name
    and* event_target_id = opt pint
    and* event_target_iid = opt pint
    and* event_target_type = opt gen_event_target_type
    and* event_author_id = pint
    and* event_target_title = opt (oneofl Corpus.waters)
    and* event_created_at = oneofl Corpus.boats
    and* event_author = opt gen_user_short
    and* event_push_data = return None
    and* event_note = return None
    and* event_wiki_page = return None
    and* event_author_username = oneofl Corpus.muppets in
    return
      {
        Gitlab_t.event_id;
        event_project_id;
        event_action_name;
        event_target_id;
        event_target_iid;
        event_target_type;
        event_author_id;
        event_target_title;
        event_created_at;
        event_author;
        event_push_data;
        event_note;
        event_wiki_page;
        event_author_username;
      }

  let event : Gitlab_t.event QCheck.arbitrary =
    make ~print:Gitlab_j.string_of_event gen_event

  let user_short : Gitlab_t.user_short QCheck.arbitrary =
    make ~print:Gitlab_j.string_of_user_short gen_user_short

  (* let webhooks : Gitlab_t.webhooks QCheck.arbitrary = *)
  (*   make ~print:Gitlab_j.string_of_webhooks gen_webhooks *)
end

let event_round_trip =
  QCheck.Test.make ~count:1000 ~name:"event round trip" Generator.event
    (fun e ->
      let event = Gitlab_j.event_of_string @@ Gitlab_j.string_of_event e in
      e = event)

let user_short_round_trip =
  QCheck.Test.make ~count:1000 ~name:"user_short round trip"
    Generator.user_short (fun e ->
      let event =
        Gitlab_j.user_short_of_string @@ Gitlab_j.string_of_user_short e
      in
      e = event)

(* let webhooks_round_trip = *)
(*   QCheck.Test.make ~count:1000 *)
(*     ~name:"webhooks round trip" *)
(*     Generator.webhooks *)
(*     (fun e -> *)
(*        let event = Gitlab_j.webhooks_of_string @@ Gitlab_j.string_of_webhooks e in *)
(*        e = event *)
(*     ) *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ event_round_trip; user_short_round_trip ]
  in
  Alcotest.run "gitlab serialisation to JSON" [ ("suite", suite) ]
