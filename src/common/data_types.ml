type version = {
  v_db: string;
  v_db_version: int;
}

type www_server_info = {
  www_apis : string list;
}

type opam_entry = {
  opam_name : string;
  opam_version : string;
  opam_synopsis : string;
}

type library_entry = {
  lib_name : string ;
  lib_opam_name : string ;
  lib_opam_version : string;
}

type meta_entry = {
  meta_name : string ;
  meta_opam : string ;
}
 
type module_entry = {
  mutable mdl_id : int32;
  mdl_name : string;
  mdl_basename: string;
  mutable mdl_path : string;
  mdl_opam_name : string;
  mdl_opam_version: string;
  mdl_libs : library_entry list;
}

type response = {
  generated : bool
}