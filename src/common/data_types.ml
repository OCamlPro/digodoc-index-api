(** Module [Data_types] defines all server-side types also used by user-side application *)

type opam_entry = {
  opam_name : string;
  opam_version : string;
  opam_synopsis : string;
}
(** Information about a package extracted from meta file. *)

type library_entry = {
  mutable lib_id : int32;
  lib_name : string ;
  lib_opam_name : string ;
  lib_opam_version : string;
}
(** Information about a library extracted from meta file. *)

type meta_entry = {
  meta_name : string ;
  meta_opam : string ;
}
(** Information about a meta extracted from meta file. *)

type module_entry = {
  mutable mdl_id : int32;
  mdl_name : string;
  mdl_basename: string;
  mutable mdl_path : string;
  mdl_opam_name : string;
  mdl_opam_version: string;
  mdl_libs : library_entry list;
  mutable mdl_vals : (string * string) list;
}
(** Information about a module, its libraries and its values 
    extracted from meta file. *)

(** Data structure that is indexed from 'ENTRY' and other meta files 
    situated under [Index.docs_dir] directory *)
type entry =
  | Module of module_entry
  | Library of library_entry
  | Opam of opam_entry
  | Meta of meta_entry

type response = {
  generated : bool
}
(** Result of [generate] service *)