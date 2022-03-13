open Db_lwt
open Data_types

(** Module [Db] regroups all the DB requests divided into module for every handler.
    Request are sended to PostgreSQL database. Every function that is supposed to be an handler's entry point
    catches PostgreSQL errors with [Misc_db.catch_db_error]. Functions use ```pgocaml_ppx``` that defines special syntax
    for SQL requests. *)

module Generate = struct

  let insert_opam {opam_name; opam_version; opam_synopsis} =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "insert into opam_index values ($opam_name, $opam_version, $opam_synopsis)"]
  (** Insert package row. *)

  let insert_lib { lib_id; lib_name; lib_opam_name; _ } =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "insert into library_index values ($lib_id, $lib_name, $lib_opam_name)"]
  (** Insert library row. *)

  let insert_meta {meta_name; meta_opam} =
    with_dbh >>> fun dbh ->
    [%pgsql dbh "insert into meta_index values ($meta_name, $meta_opam)"]
  (** Insert meta row. *)

  let insert_module
    {mdl_id; mdl_name; mdl_path; mdl_opam_name; mdl_libs; mdl_vals; mdl_types; mdl_classes} =
    with_dbh >>> fun dbh ->
    (* insert module row *)
    [%pgsql dbh "insert into module_index values ($mdl_id, $mdl_name, $mdl_path, $mdl_opam_name)"]
    >>= function () -> Lwt_list.iter_s (fun lib ->
      (* insert a row for every module's library *)
      [%pgsql dbh "insert into module_libraries values ($mdl_id, ${lib.lib_id}, ${lib.lib_name})"] ) mdl_libs
    >>= function () -> Lwt_list.iter_s (fun (ident,vall) ->
      (* insert a row for every module's value *)
      [%pgsql dbh "insert into module_vals values ($mdl_id, $mdl_name, $mdl_opam_name, $ident, $vall)"] ) mdl_vals
    >>= function () -> Lwt_list.iter_s (fun {type_id; ident;constructors} ->
      [%pgsql dbh "insert into module_types values ($mdl_id, $mdl_name, $mdl_opam_name, $type_id, $ident)"]
      (* insert a row for every module's types *)
      >>= function () ->
      Lwt_list.iter_s (fun cons->
        [%pgsql dbh "insert into type_signatures values ($type_id, $cons)"]
      (* insert a row for every types' signature *)
      ) constructors
    ) mdl_types;

    >>= function () -> Lwt_list.iter_s (fun {type_id; ident; is_class_type; constructors} ->
      match is_class_type with
      | Some false -> [%pgsql dbh "insert into module_classes values ($mdl_id, $mdl_name, $mdl_opam_name, FALSE, $type_id, $ident)"]
      | Some true -> [%pgsql dbh "insert into module_classes values ($mdl_id, $mdl_name, $mdl_opam_name, TRUE, $type_id, $ident)"]
      | _ -> failwith "should not occur"
      (* insert a row for every module's classes *)
      >>= function () ->
      Lwt_list.iter_s (fun cons->
        [%pgsql dbh "insert into class_signatures values ($type_id, $cons)"]
        (* insert a row for every class' contents *)
      ) constructors
    ) mdl_classes
  (** Insert module row and information about its libraries, its values, and its declared types
      into a corresponding DB table. *)
end
(** Module that regroups all DB requests for [Handlers.generate] handler. *)
