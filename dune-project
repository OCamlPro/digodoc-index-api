(lang dune 2.4)
(name digodoc-index-api)

(source (github OCamlPro/digodoc-index-api))

; TODO : ocamlpro licence
(license MIT)

(authors "Mohamed Hernouf")
(maintainers "mohamed.hernouf@ocamlpro.com")
(homepage https://github.com/OCamlPro/digodoc-index-api)
(bug_reports https://github.com/OCamlPro/digodoc-index-api/issues)

(generate_opam_files true)

(package
 (name digodoc-index-api)
 (synopsis "Index API server for digodoc DB")
 (depends
  ez_api
  ez_file
  odoc
  merlin
  dune
  calendar
  geoip
  lwt_ppx
  cohttp-lwt-unix
  websocket-lwt-unix
  tls
  cohttp-lwt-jsoo
  re
  (pgocaml_ppx ( >= 4.3.0))
  ez_pgocaml
  (ppx_deriving_encoding ( >= 0.2.2 ))
  ))
