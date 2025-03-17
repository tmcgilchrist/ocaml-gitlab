Gitlab bindings for OCaml
==========
[![GitHub CI][github-shield]][github-ci] [![docs][docs-shield]][docs]
[![OCaml-CI Build Status][ocaml-ci-shield]][ocaml-ci]

Native OCaml bindings to [Gitlab REST API v4].

The API coverage is incomplete (currently we support many of the Commit, Project, User and Event
APIs) but if you find something missing please create an Issue or PR.

Pre-requisites
----------

 * Plain opam / ocaml for building.
 
Configuration
----------

Gitlab can be hosted in multiple places and configurations. By default the library uses
the public `gitlab.com` site with it's API endpoint of `https://gitlab.com/api/v4`. This
can be changed with:

    GITLAB_URL # to configure the GitLab instance to connect to.

or programatically by overriding the `Env` module.


Debugging
----------
Two environment variables will cause more debugging to be output:

    GITLAB_DEBUG=1   # API calls output to stderr
    COHTTP_DEBUG=1   # even more HTTP-level debugging

If using the bindings from the toplevel, you can also set `Gitlab.log_active`
to `true` to get the same effect as setting the `GITLAB_DEBUG` environment
variable.

Lab Cli
----------

A command line client is provided called `lab` in the style of a similarly named
tool `hub` for GitHub. From source you can run as:

``` shell
dune exec -- lab <arguments>
```

If you opam install lab then the executable is available as `lab`.


Run `lab -h` for more information about cli options.

Tests
------

Running mdx tests for lab cli:

``` shell
# Run integration tests against real gitlab instance
dune runtest --profile non-deterministic

# Promote any changes
dune runtest --profile non-deterministic --auto-promote
```

NOTE that ocaml-ci doesn't run these tests, any changes to the cli need to be validated manually.

 [github-shield]: https://github.com/tmcgilchrist/ocaml-gitlab/actions/workflows/ci.yaml/badge.svg
 [github-ci]: https://github.com/tmcgilchrist/ocaml-gitlab/actions/workflows/ci.yaml

 [docs-shield]:https://img.shields.io/badge/doc-online-blue.svg
 [docs]: https://tmcgilchrist.github.io/ocaml-gitlab/gitlab/index.html

 [ocaml-ci]: https://ci.ocamllabs.io/github/tmcgilchrist/ocaml-gitlab
 [ocaml-ci-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Ftmcgilchrist%2Focaml-gitlab%2Fmaster&logo=ocaml
 [Gitlab REST API v4]: https://docs.gitlab.com/ee/api/README.html

Resources
----------

 [MDX Setup in RWO](https://github.com/realworldocaml/book/blob/master/Makefile#L14)

## DEBUGGING

If you get this error when running test or example code
`Fatal error: exception (Failure "No SSL or TLS support compiled into Conduit")`

You need to install the ssl versions of your chosen async framework

`opam install lwt_ssl async_ssl`
