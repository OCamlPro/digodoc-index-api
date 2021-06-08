(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

open EzFile.OP
open EzCompat
open Data_types
open Db

type entry =
  | Module of module_entry
  | Library of library_entry
  | Opam of opam_entry
  | Meta of meta_entry


let module_cut m =
  let rec iter m i len =
    if i+1 = len then
      m, ""
    else
    if m.[i] = '_' && m.[i+1] = '_' then
      (* Don't forget to capitalize (to handle for instance Stdlib__map) *)
      String.sub m 0 i, String.capitalize (String.sub m (i+2) (len - i - 2))
    else
      iter m (i+1) len
  in
  iter m 0 (String.length m)

let pkg_of_opam opam =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s@%s.%s"
    lib.lib_name lib.lib_opam_name lib.lib_opam_version

let pkg_of_mdl mdl =
  let version = mdl.mdl_opam_version in
  match mdl.mdl_libs with
  | lib :: _rem -> pkg_of_lib lib
  | [] ->
      let pack, alias = module_cut mdl.mdl_basename in
      if alias = "" then
        Printf.sprintf "MODULE.%s@%s.%s"
          mdl.mdl_basename mdl.mdl_opam_name version
      else
        let pkg =
          Printf.sprintf "MODULE.%s__@%s.%s" pack mdl.mdl_opam_name version in
        if Sys.file_exists (PConfig.digodoc_dir // pkg) then
          pkg
        else
          Printf.sprintf "MODULE.%s@%s.%s" pack mdl.mdl_opam_name version

let library_of_string s =
  let lib_name, s = EzString.cut_at s '@' in
  let lib_opam_name, lib_opam_version = EzString.cut_at s '.' in
  { lib_name ; lib_opam_name ; lib_opam_version }

let read_entry file =
  match EzFile.read_lines_to_list file with
  |
    "opam" ::
    opam_name ::
    opam_version ::
    opam_synopsis ->
      let opam_synopsis = String.concat " " opam_synopsis in
      Opam { opam_name ; opam_version ; opam_synopsis }
  | [
    "meta" ;
    meta_name ;
    meta_opam ;
    _ ;
  ] -> Meta { meta_name ; meta_opam  }
  | [
    "library" ;
    lib_name ;
    lib_opam_name ;
    lib_opam_version ;
  ] -> Library { lib_name ; lib_opam_name ; lib_opam_version }
  | "module" ::
    mdl_name ::
    mdl_opam_name ::
    mdl_opam_version ::
    mdl_basename ::
    mdl_libs ->
      let mdl_libs = List.map library_of_string mdl_libs in
      Module { mdl_id = Int32.of_int 0; mdl_path = ""; mdl_name ; mdl_opam_name ; mdl_opam_version ;
               mdl_basename ; mdl_libs }
  | _lines ->
      Printf.eprintf "Unrecognized format for entry file %S\n%!" file;
      raise Not_found


let fill_opam_index state =
  Lwt_list.iter_s (function 
    | Opam opam ->
      insert_opam opam
    | _ -> Lwt.return_unit
    ) state

let fill_library_index state =
  Lwt_list.iter_s (function 
    | Library lib ->
      insert_lib lib
    | _ -> Lwt.return_unit
    ) state


let fill_meta_index state =
  Lwt_list.iter_s (function 
    | Meta meta ->
      insert_meta meta
    | _ -> Lwt.return_unit
    ) state

let fill_module_index state =

  let cpt = ref 0 in

  let add_module pack alias mdl =
    let pkg = pkg_of_mdl mdl in

    let mdl_path, _ =
      if alias = "" then
        Printf.sprintf "%s/%s/index.html" pkg mdl.mdl_name, mdl.mdl_name
      else
        (* In general, when we have a packed module M__N,
           M is generated and contains an alias N = M__N.
           However, when M already exists (written by the user),
           then the generated module is called M__. *)
        let path = Printf.sprintf "%s/%s__/%s/index.html" pkg pack alias in
        if Sys.file_exists (PConfig.digodoc_dir // path) then
          path, Printf.sprintf "%s__.%s" pack alias
        else
          Printf.sprintf "%s/%s/%s/index.html" pkg pack alias,
          Printf.sprintf "%s.%s" pack alias
    in

    (* DB *)
    let mdl_id = Int32.of_int !cpt in
    mdl.mdl_id <- mdl_id;
    mdl.mdl_path <- mdl_path;
    cpt:= !cpt + 1;
    insert_module mdl

  in

  Lwt_list.iter_s (function
      | Module mdl ->
          let pack, alias = module_cut mdl.mdl_name in
          add_module pack alias mdl
      | _ -> Lwt.return_unit
    ) state

let read_all_entries () =
  let entries = ref [] in
  let dir = PConfig.digodoc_dir in
  Array.iter (fun pkg ->
      let dir = dir // pkg in
      Array.iter (fun file ->

          if EzString.starts_with file ~prefix:"ENTRY." then
            let entry = read_entry ( dir // file ) in
            entries := entry :: !entries

        ) ( try Sys.readdir dir with _ -> [||] )
    ) ( Sys.readdir dir ) ;

  Printf.eprintf "%d entries read\n%!" ( List.length !entries ) ;
  !entries

let generate () =
  Printf.eprintf "Generating index...\n%!";
  let state = read_all_entries () in
  let%lwt () = fill_opam_index state in 
  let%lwt () =  fill_library_index state in 
  let%lwt () = fill_meta_index state in 
  let%lwt () = fill_module_index state in
  let _ = Lwt_io.eprintf "Index generation done.\n%!" in 
  Lwt.return_true
