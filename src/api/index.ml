
(** Module that defines all indexation functions subsequent DB completion. *)

open EzFile.OP
open EzCompat
open Data_types
open Ez_search.V1

(** {1 Documentation} *)

let docs_dir = PConfig.digodoc_dir // "docs"
(** Documentation directory that will be indexed *)

let module_cut m =
  let rec iter m i len =
    if i+1 = len then
      m, ""
    else
    if m.[i] = '_' && m.[i+1] = '_' then
      String.sub m 0 i, String.capitalize (String.sub m (i+2) (len - i - 2))
    else
      iter m (i+1) len
  in
  iter m 0 (String.length m)
(** Cuts module names that are formed [Packname__modulename] into [(Packname__modulename, modulename)] *)

let pkg_of_opam (opam:Data_types.opam_entry) =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version
(** [pkg_of_opam opam] computes the path to the directory that contains documentation for [opam]. *)

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s@%s.%s"
    lib.lib_name lib.lib_opam_name lib.lib_opam_version
(** [pkg_of_lib lib] computes the path to the directory that contains documentation for [lib]. *)

let pkg_of_mdl mdl =
  (* TODO: Treat Lib__Module as Lib.Module *)
  let version = mdl.mdl_opam_version in
  match mdl.mdl_libs with
  (* if module has an associated library *)
  | lib :: _rem -> pkg_of_lib lib
  (* if module is "orphan" *)
  | [] ->
      let pack, alias = module_cut mdl.mdl_basename in
      (* if module name doesn't contain '__' *)
      if alias = "" then
        Printf.sprintf "MODULE.%s@%s.%s"
          mdl.mdl_basename mdl.mdl_opam_name version
      else
        let pkg =
          Printf.sprintf "MODULE.%s__@%s.%s" pack mdl.mdl_opam_name version in
        if Sys.file_exists (docs_dir // pkg) then
          pkg
        else
          Printf.sprintf "MODULE.%s@%s.%s" pack mdl.mdl_opam_name version
(** [pkg_of_mdl mdl] computes the path to the directory that contains documentation for [mdl]. *)

let library_of_string s =
  let lib_name, lib_opam = EzString.cut_at s '@' in
  let lib_opam_name, lib_opam_version = EzString.cut_at lib_opam '.' in
  { lib_id = Int32.of_int 0; lib_name ; lib_opam_name ; lib_opam_version }
(** Constructs [Data_types.library_entry] from string of form
    "<lib_name>@<lib_opam_name>.<lib_opam_version>" *)

let read_entry file =
  match EzFile.read_lines_to_list file with
  (* file contains package info *)
  | "opam" ::
    opam_name ::
    opam_version ::
    opam_synopsis ->
      let opam_synopsis = String.concat " " opam_synopsis in
      Opam { opam_name ; opam_version ; opam_synopsis }
  (* file contains meta info *)
  | [ "meta" ;
    meta_name ;
    meta_opam ;
    _
  ] -> Meta { meta_name ; meta_opam  }
  (* file contains library info *)
  | [ "library" ;
    lib_name ;
    lib_opam_name ;
    lib_opam_version
  ] -> Library { lib_id = Int32.of_int 0 ; lib_name ; lib_opam_name ; lib_opam_version }
  (* file contains module info *)
  | "module" ::
    mdl_name ::
    mdl_opam_name ::
    mdl_opam_version ::
    mdl_basename ::
    mdl_libs ->
      let mdl_libs = List.map library_of_string mdl_libs in
      Module { mdl_id = Int32.of_int 0; mdl_path = ""; mdl_name ; mdl_opam_name ; mdl_opam_version ;
        mdl_basename ; mdl_libs; mdl_vals = [] ; mdl_types = []}
  | _lines ->
      Printf.eprintf "Unrecognized format for entry file %S\n%!" file;
      raise Not_found
(** Reads meta file ('ENTRY._') and returns [entry] corresponding to the content *)

let read_val file =
  match EzFile.read_lines_to_list file with
  | _ ::
    _ ::
    _ ::
    mdl_vals ->
      (* Translation [x1;y1;x2;y2;...] to [(x1,y1);(x2,y2);...] *)
      let rec separate l acc =
        match l with
        | [] -> List.rev acc
        | x::y::ll -> separate ll ((x,y)::acc)
        | _ -> failwith "should not occur"
      in
        separate mdl_vals []
  | _lines ->
      Printf.eprintf "Unrecognized format for val file %S\n%!" file;
      raise Not_found
(** Reads meta file ('VAL.MODULE._') and returns list of couples that describes
    the values (element of ocaml language). First element is value's name and second
    is its type. *)

let read_types file =
  begin
  match EzFile.read_lines_to_list file with
  | _ ::
    _ ::
    _ ::
    mdl_types ->
    (* Translation [identifier_x1;type_nature_y1;signature_z1;x2;y2;z2...] to [(x1,y1,z1);(x2,y2,z2);...] *)
      let rec separate acc = function
        | [] -> List.rev acc
        | ident::typ::sign ->

            let get_signature_and_continue =
              let rec aux_get_sig acc =
                function
                  | [] -> List.rev acc, []
                  | x::r ->
                      match x with
                      | "" -> List.rev acc, r
                      | s -> aux_get_sig (s::acc) r
                in fun file_sig -> aux_get_sig [] file_sig
              in

            let signature, rest_of_file =
              get_signature_and_continue sign
            in
            separate ((ident, typ, signature)::acc) rest_of_file

        | _s ->
            Format.printf "SEPARATING TYPES FOR %s" file;
            failwith "should not occur"
      in
        separate [] mdl_types
  | _lines ->
    Printf.eprintf "Unrecognized format for TYPE_MODULE file %S\n%!" file;
      raise Not_found
  end
(** Reads meta file ('TYPES.MODULE._') and returns list of triplets that describe
    the type signatures (element of ocaml language).
    First element is type's name,
    Second is the nature of its type (TYPE_ABSTRACT, TYPE_CLASS, TYPE_VARIANT,...).
      This might be brought to change at some point for no
    third is the actual signature. *)

let type_id_cnt =
  let count = ref (-1) in
  fun () ->
    incr count;
    !count

let list_type_declarations mdl file =
  let init_type mdl ident =
      let type_id = Int32.of_int (type_id_cnt ()) in
      let mdl_id = mdl.mdl_id in
      let mdl_name = mdl.mdl_name in
      let opam_name = mdl.mdl_opam_name in
      {type_id; mdl_id; mdl_name; opam_name; ident; constructors = "";}
  in
  let types =
    List.map (fun (ident, typkind, typesig) ->
      let given_type = init_type mdl ident in
      let concat = String.concat "\n" in
      match typkind with
      | "TYPE_OPEN" ->
          { given_type with constructors = "..." }

      | "TYPE_ABSTRACT" ->
          { given_type with constructors = concat typesig }

      | "TYPE_RECORD" ->
          { given_type with constructors = concat @@ ["{ "] @ typesig @ [" }"] }
          (* record fields are yet to be indexed, they could be by splitting on ':' *)
      | "CLASS_OBJ" ->
          { given_type with constructors = concat @@ ["object"] @ typesig @ ["end"] }

      | "TYPE_VARIANT" ->
          {
            given_type with constructors =
              concat @@ List.map (fun const ->
                match String.split_on_char '\\' const with
                | [] -> assert false
                | [x] -> x
                | l -> Format.asprintf "%a" ( Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt " of ")
                Format.pp_print_string ) l
              ) typesig
          }

      | _s ->
          Format.printf "TYPEKIND ERROR in file %s@. Type declaration preceding %s = %a@." file ident
              ( Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
                Format.pp_print_string ) typesig;
          failwith "TYPES.m type kind should not occur"
      ) (read_types file)
  in mdl.mdl_types <- types
  (** This is where all subsequent indexations could be made, are accessible here :
    - Record fields names and their types
    - Constructors and their arguments
    - Class object and their contents, methods/val/constraints
    - Abstract types
    For now, only identifiers will be indexed
    *)

let fill_opam_index state =
  Lwt_list.iter_s (function
    | Opam opam ->
      Db.Generate.insert_opam opam
    | _ -> Lwt.return_unit
    ) state
(** Iterate over list of [entry] in order to fill DB table for packages. *)

let fill_library_index state =
  let cpt = ref 0 in
  Lwt_list.iter_s (function
    | Library lib ->
      let lib_id = Int32.of_int !cpt in
      lib.lib_id <- lib_id;
      cpt:= !cpt + 1;
      Db.Generate.insert_lib lib
    | _ -> Lwt.return_unit
    ) state
(** Iterate over list of [entry] in order to fill DB table for librareis. *)

let fill_meta_index state =
  Lwt_list.iter_s (function
    | Meta meta ->
      Db.Generate.insert_meta meta
    | _ -> Lwt.return_unit
    ) state
(** Iterate over list of [entry] in order to fill DB table for metas. *)

let fill_module_index state =
  let cpt = ref 0 in
  let add_module pack alias mdl =
    let pkg = pkg_of_mdl mdl in
    (* TODO: to store correctly module name *)
    let mdl_path, mdl_name =
      if alias = "" then
        Printf.sprintf "docs/%s/%s/index.html" pkg mdl.mdl_name, mdl.mdl_name
      else
        (* In general, when we have a packed module M__N,
           M is generated and contains an alias N = M__N.
           However, when M already exists (written by the user),
           then the generated module is called M__. *)
        let path = Printf.sprintf "docs/%s/%s__/%s/index.html" pkg pack alias in
        if Sys.file_exists (docs_dir // path) then
          path, Printf.sprintf "%s__.%s" pack alias
        else
          Printf.sprintf "docs/%s/%s/%s/index.html" pkg pack alias,
          Printf.sprintf "%s.%s" pack alias
    in
    let mdl_id = Int32.of_int !cpt in
    mdl.mdl_id <- mdl_id;
    mdl.mdl_path <- mdl_path;
    mdl.mdl_name <- mdl_name;
    cpt:= !cpt + 1;
    (* Find id for every library *)
    List.iter (fun lib1 ->
      List.iter (fun entry ->
        match entry with
        (* Looks up for library entry that has the same name and package *)
        | Library lib2 when String.equal lib1.lib_name lib2.lib_name
                         && String.equal lib1.lib_opam_name lib2.lib_opam_name ->
          lib1.lib_id <- lib2.lib_id
        | _ -> ()
          ) state) mdl.mdl_libs;
    Db.Generate.insert_module mdl
  in
    Lwt_list.iter_s (function
        | Module mdl ->
          let pack, alias = module_cut mdl.mdl_name in
          add_module pack alias mdl
        | _ -> Lwt.return_unit
      )
      state
(** Iterate over list of [entry] in order to fill DB table for modules
    and modules' libraries. *)

let read_all_entries () =
  let entries = ref [] in
  Array.iter (fun pkg ->
    let dir = docs_dir // pkg in
    Array.iter (fun file ->
      (* Read 'ENTRY._' file *)
      if EzString.starts_with file ~prefix:"ENTRY." then
        let entry = read_entry ( dir // file ) in
        begin
          match entry with
          (* If meta file contains a module information *)
          | Module mdl ->
            let vals_file = "VALS." ^ snd @@ EzString.cut_at file '.' in
            (* Read corresponding 'VAL.MODULE._' and extract all modules' values *)
            if EzFile.exists (dir // vals_file) then (
              let vals = read_val (dir // vals_file) in
              mdl.mdl_vals <- vals );

            let types_file = "TYPES." ^ snd @@ EzString.cut_at file '.' in
            (* Read corresponding 'TYPES.MODULE._' and extract from all modules' type (and cass) declarations, their signatures *)
            if EzFile.exists (dir // types_file) then
              list_type_declarations mdl (dir // types_file)

          | _ -> ()
        end;
        entries := entry :: !entries
        )
      (try Sys.readdir dir with _ -> [||])
    )
    (Sys.readdir docs_dir) ;
  Printf.eprintf "%d entries read\n%!" ( List.length !entries ) ;
  !entries
(** Iterates over [docs_dir] directory and extracts all found
    information from meta files in order to create corresponding [entry].
    Every module should have additional file 'VAL.MODULE._' that lists all
    module's values and their types. *)

let generate () =
  Lwt.catch
    (fun () ->
      Printf.eprintf "Generating index...\n%!";
      let state = read_all_entries () in
      let%lwt () = fill_opam_index state in
      let%lwt () =  fill_library_index state in
      let%lwt () = fill_meta_index state in
      let%lwt () = fill_module_index state in
      let _ = Lwt_io.eprintf "Index generation done.\n%!" in
      Lwt.return_true
    )
    (function _ -> Lwt.return_false)

(** Main indexation function that indexates :
    - packages
    - libraries
    - metas
    - modules with associated th them libraries
    - modules' values
    Sources aren't indexed because all information about them
    could be get from package entry. If indexation stage doesn't
    occur errors then returns true otherwise returns false. *)

(** {1 Sources} *)

(** {1 Documentation } *)

let sources_dir = PConfig.digodoc_dir // "sources_files"
(** Sources directory that will be indexed *)

let sources_db_dir = PConfig.digodoc_dir // "sources_db"
(** Place where API will safe indexation artefacts *)

let ocaml_db_name = "ocaml_db"
(** Place where API will safe indexation artefacts for .ml, .mli, .mll, .mly *)

let dune_db_name = "dune_db"
(** Place where API will safe indexation artefacts for dune files *)

let makefile_db_name = "makefile_db"
(** Place where API will safe indexation artefacts for Makefiles *)

let select_ocaml path =
  let basename = Filename.basename path in
  let _, ext = EzString.rcut_at basename '.' in
  match ext with
  | "ml" | "mll" | "mly" | "mli" -> true
  | _ -> false
(** Function that says if filename under the [path] is one of specific for OCaml files *)

let select_dune path =
  let basename = Filename.basename path in
  match basename with
  | "dune" | "dune-project" | "dune-workspace" -> true
  | _ -> false
(** Function that says if filename under the [path] is one of specific for Dune files *)

let select_makefile path =
  let basename = Filename.basename path in
  match String.lowercase_ascii basename with
  | "makefile" -> true
  | _ -> false
(** Function that says if filename under the [path] is one of specific for Dune files *)

let sources () =
  Lwt.catch
    (fun () ->
      EzFile.make_dir sources_db_dir;
      Printf.eprintf "Indexating ocaml files...\n%!";
      (* Indexate ocaml files *)
      EzSearch.index_directory
        ~db_dir:sources_db_dir
        ~select:select_ocaml
        ~db_name:ocaml_db_name
        sources_dir;
      (* Indexate dune files *)
      Printf.eprintf "Indexating dune files...\n%!";
      EzSearch.index_directory
        ~db_dir:sources_db_dir
        ~select:select_dune
        ~db_name:dune_db_name
        sources_dir;
      (* Indexate Makefiles *)
      Printf.eprintf "Indexating makefiles...\n%!";
      EzSearch.index_directory
        ~db_dir:sources_db_dir
        ~select:select_makefile
        ~db_name:makefile_db_name
        sources_dir;
      Printf.eprintf "Indexation done.\n%!";
      Lwt.return_true
    )
    (function _ -> Lwt.return_false)
(** Sources indexation function. Uses ```ez_search``` package that provides functionalities to create
    DB with sources and to make different type of search inside. If indexation stage doesn't occur
    errors then returns true otherwise returns false.*)
