open Data_types
open Encoding
open EzAPI

(** Module [Services] defines all the services with their arguments, results and errors JSON encodings. *)

let section_indexation = Doc.section "Indexation"

let sections = [ section_indexation ]
(** All documentation sections. Every service should be associated to one of them in order to appear in API documentation. *)

let generate : (response, exn, no_security) service0 =
  service
    ~section:section_indexation
    ~name:"generate"
    ~descr:"Indexate all digodoc entries and fills digodoc DB."
    ~output:response_enc
    Path.(root // "generate")
(** Service that indexates and generates entries for digodoc DB. 
    Entries are extracted from special meta files generated previously 
    by digodoc under the directory that should be specified in [PConfig.digodoc_dir].
    Returns [Data_types.response] that indicates either indexation was succesful or not. *)