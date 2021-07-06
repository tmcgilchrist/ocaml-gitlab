(lang dune 1.10)

(name gitlab)

(generate_opam_files true)

(license BSD-3-clause)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist" )
(source (github tmcgilchrist/ocaml-gitlab))
(documentation "https://tmcgilchrist.github.io/ocaml-gitlab/")

(package
 (name gitlab)
 (depends
  (ocaml (>= 4.04.0))
  (dune (>= 1.10))
  (uri (>= 1.9.0))
  (cohttp (>= 4.0))
  (lwt (>= 2.4.4))
  (atdgen (>= 2.0.0))
  (yojson (>= 1.6.0))
  stringext)
 (synopsis "Gitlab APIv4 OCaml library")
 (description "This library provides an OCaml interface to the
[Gitlab APIv4](https://docs.gitlab.com/ee/api/) (JSON).

It is compatible with [MirageOS](https://mirage.io) and also compiles to pure
JavaScript via [js_of_ocaml](http://ocsigen.org/js_of_ocaml)."))

(package
 (name gitlab-unix)
 (depends
  (ocaml (>= 4.04.0))
  (dune (>= 1.10))
  (gitlab (= :version))
  (cohttp (>= 4.0))
  (cohttp-lwt-unix (>= 4.0))
  (lwt (>= 2.4.4))
  stringext
  (cmdliner (>= 0.9.8))
  base-unix)
 (synopsis "Gitlab APIv4 OCaml library")
 (description "This library provides an OCaml interface to the
[Gitlab APIv4](https://docs.gitlab.com/ee/api/) (JSON).
This package installs the Unix (Lwt) version."))
