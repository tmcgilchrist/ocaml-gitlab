# Unreleased

## Bug fixes

 * `gitlab.atd`: add `scheduler_failure` to `failure_reason`

# 0.1.7 - 2023-02-02

 * Drop ezjsonm dependency for `gitlab` (#76 @tmcgilchrist)

## Bug fixes
 * `gitlab.atd`: `description` is nullable (#75 @maiste)

# 0.1.6 - 2022-11-08

## Added

 * `gitlab.atd`: use `date_time` instead of strings for `_at` fields (#68 @arvidj)
 * `Project`: add `merge_request_pipelines` and `by_short_ref` (#68 @arvidj)
 * `Project.merge_requests`: add parameters `updated_after`, `updated_before`,
   `created_after`, `created_before` and `order_by` (#68 @arvidj)

## Bug fixes

 * `Project.pipeline_jobs`: 404 means pending pipeline, not error (#68 @arvidj)
 * `gitlab.atd`: `time_stats` estimates are strings (#66 @MisterDA)

# 0.1.5 - 2022-06-17

## Added

  * Depend on atd >= 2.8 to get codegen fixes. (#63 @MisterDA)
  * Add endpoints `Project.pipeline`, `Project.pipeline_jobs` , `Project.job_trace` , `Project.pipelines`.
  * Add param `updated_after` to `Project.merge_requests`

## Bug fixes

  * Make the field `source_project_id` of `merge_request` nullable

# 0.1.4 - 2022-06-02

## Added

 * Add support for project hooks (#58 @novemberkilo)
 * Support listing and creating Notes in merge requests. (#59 @MisterDA)
 * Parse more webhooks and events including project, job, deployment and feature flag (#53 @tmcgilchrist)

## Bug fixes

 * Documentation fixes (#53 @tmcgilchrist) (#57 @MisterDA)

# 0.1.3 - 2022-04-04

## Added
 * Various changes to rework lab cli client. (#50 #51 @maiste @tmcgilchrist @OlivierNicole)

## Bug fixes
 * Strip api/v4 from authorize and token url during OAuth. (#48 @tmcgilchrist)
 * Serialise milestone_id is an int. (#48 @tmcgilchrist)
 * Various merge_request webhook deserialisation fixes. (#47 @tmcgilchrist)
 * Fix incorrect field name, detected with atdgen 2.3.x (#46 @mjambon)
 * Fix commit_short_webhook prefix typo (#45 @MisterDA)
 * Update GHA to use windows-2022  (#44 @MisterDA)
 * Fix subcommands names and document env vars (#44 @MisterDA)
 * Document GITLAB_ env vars via Cmdliner (#44 @MisterDA)
 * Update cmdliner 1.1.0 API (#41 @MisterDA)

# 0.1.2 - 2022-02-02

## Added

- Revert using int64 types in gitlab.atd (#38 @tmcgilchrist)

# 0.1.1 - 2022-01-14

## Added

- Support Authorization code flow in OAuth. (#30 @tmcgilchrist)
- Extra merge status values (#32 @tmcgilchrist)

# 0.1.0 - 2021-11-17

## Added

- Initial release of gitlab bindings. Supports Personal and Project Access Tokens, and selected
  parts of the Project, User, Events and Group APIs.
