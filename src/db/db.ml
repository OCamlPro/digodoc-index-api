open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

let get_version () =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "select value from ezpg_info where name = 'version'"]
  >|= version_of_rows

let insert_opam {opam_name; opam_version; opam_synopsis} =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "insert into opam_index values ($opam_name, $opam_version, $opam_synopsis)"] 

let insert_lib { lib_id; lib_name; lib_opam_name; _ } =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "insert into library_index values ($lib_id, $lib_name, $lib_opam_name)"] 

let insert_meta {meta_name; meta_opam} =
  with_dbh >>> fun dbh ->
  [%pgsql dbh "insert into meta_index values ($meta_name, $meta_opam)"] 

let insert_module 
  {mdl_id; mdl_name; mdl_path; mdl_opam_name; mdl_libs; _} =
  with_dbh >>> fun dbh ->
  let%lwt _ = [%pgsql dbh "insert into module_index values ($mdl_id, $mdl_name, $mdl_path, $mdl_opam_name)"] in
  Lwt_list.iter_s (fun lib ->
    [%pgsql dbh "insert into module_libraries values ($mdl_id, ${lib.lib_id}, ${lib.lib_name})"] ) mdl_libs
