(lang dune 2.9)
(name gitlab)
(generate_opam_files true)
(using mdx 0.1)

(license BSD-3-clause)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist" )
(source (github tmcgilchrist/ocaml-gitlab))
(documentation "https://tmcgilchrist.github.io/ocaml-gitlab/")

(package
 (name gitlab)
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 1.10))
  (uri (>= 1.9.0))
  (cohttp (>= 4.0))
  (lwt (>= 2.4.4))
  (atdgen (>= 2.0.0))
  (yojson (>= 1.7.0))
  stringext)
 (synopsis "GitLab APIv4 OCaml library")
 (description "This library provides an OCaml interface to the
[GitLab APIv4](https://docs.gitlab.com/ee/api/) (JSON).

It is compatible with [MirageOS](https://mirage.io) and also compiles to pure
JavaScript via [js_of_ocaml](http://ocsigen.org/js_of_ocaml)."))

(package
 (name gitlab-unix)
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 1.10))
  (gitlab (= :version))
  (cohttp (>= 4.0))
  (cohttp-lwt-unix (>= 4.0))
  (tls (>= 0.11.0))
  (lwt (>= 2.4.4))
  stringext
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)

  base-unix)
 (synopsis "GitLab APIv4 OCaml library")
 (description "This library provides an OCaml interface to the
[Gitlab APIv4](https://docs.gitlab.com/ee/api/) (JSON).
This package installs the Unix (Lwt) version."))

(package
 (name lab)
 (depends
  (ocaml (>= 4.08.0))
  (mdx :with-test)
  (cmdliner (>= 0.9.8)))
 (synopsis "GitLab cli")
)