Gitlab bindings for OCaml
==========
[![GitHub CI][github-shield]][github-ci]

Native OCaml bindings to Gitlab REST API v4 see https://docs.gitlab.com/ee/api/README.html

Also see https://hackage.haskell.org/package/gitlab-haskell for the Haskell version.

TODO

   * [x] Make basic skeleton for library with ATD and a simple endpoint
   * [x] Add CI
   * [x] Add basic cli mdx test
   * [X] Add Steaming responses / pagination support
   * [ ] Add test for authenticated queries
   * [X] Parameterise gitlab url to support various gitlab hosting arrangements
   * [X] documentation introduction on how to use the library
   * [ ] Functor across HTTP libraries, Cohttp / httpaf ???

Support for ocurrent

   * [X] Webhooks integration for delivering Merge Request open, merge to master
   * [X] Support events serialisation, add tests for de-serialising events
   * [ ] CRUD for Merge requests
   * [ ] Authenticated user support via tokens
   * [ ] Authenticated user support via oauth


Pre-requisites
----------

 * opam / ocaml
 * jd for diffing json (github.com/josephburnett/jd)

Lab Cli
----------

A command line client is provided called `lab` in the style of a similarly named
tool `hub` for GitHub. From source you can run as:

``` shell
dune exec cli/lab.exe -- <arguments>
```

If you opam install lab then the executable is available as `lab`.

GITLAB_DEBUG=[true|false] for printing debug information like what HTTP requests and responses occur.
GITLAB_URL to configure the GitLab instance to connect to. This defaults to `https://gitlab.com/api/v4` on the public `https://github.com` site.

Run `lab -h` for more information about cli options.


 [github-shield]: https://github.com/tmcgilchrist/ocaml-gitlab/actions/workflows/ci.yaml/badge.svg
 [github-ci]: https://github.com/tmcgilchrist/ocaml-gitlab/actions/workflows/ci.yaml
