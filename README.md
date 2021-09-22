Gitlab bindings for OCaml
==========

Native OCaml bindings to Gitlab REST API v4 see https://docs.gitlab.com/ee/api/README.html

Also see https://hackage.haskell.org/package/gitlab-haskell for the Haskell version.

TODO

   * [x] Make basic skeleton for library with ATD and a simple endpoint
   * [x] Add CI
   * [ ] Add basic cli cram test
   * [X] Add Steaming responses / pagination support
   * [ ] Add test for authenticated queries
   * [ ] Parameterise gitlab url to support various gitlab hosting arrangements
   * [ ] documentation introduction on how to use the library
   * [ ] Functor across HTTP libraries, Cohttp / httpaf ???

Support for ocurrent

   * [ ] Webhooks integration for delivering Merge Request open, merge to master
   * [ ] Support events serialisation, add tests for de-serialising events
   * [ ] CRUD for Merge requests
   * [ ] Authenticated user support via tokens / oauth ???


Pre-requisites
----------

 * opam / ocaml
 * jd for diffing json (github.com/josephburnett/jd)