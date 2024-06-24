(lang dune 3.16)

(name needlework_scenario_generator)

(generate_opam_files true)

(source
 (github proof-ninja/needlework_scenario_generator))

(authors "Yoshihiro Imai")

(maintainers "Yoshihiro Imai")

(license MIT)

(documentation https://github.com/proof-ninja/needlework_scenario_generator)

(package
 (name needlework_scenario_generator)
 (synopsis "Needlework scenario testing generator")
 (description "A longer description")
 (depends ocaml dune xml-light csv ipaddr dolog)
 (tags
  (ocaml needlework testing)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
