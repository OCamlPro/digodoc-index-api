(** Module that stores and updates/downgrades differrent versions of DB. 
    The verions should correspond for every digodoc's API. *)

let cver = ref 0
let upgrades : (int * (unit PGOCaml.t -> int -> unit)) list ref = ref []
let downgrades: (int * string list) list ref = ref []

let register_version ?version
    ?(before_upgrade = fun _ -> ())
    ?(after_upgrade = fun _ -> ())
    ~downgrade
    ~upgrade () =
  let previous_version = !cver in
  let version = match version with
    | None -> !cver + 1
    | Some v ->
      if v <= !cver then
        Format.ksprintf failwith "Registering version %d forbidden (min %d)"
          v (!cver + 1);
      v in
  let upgrade dbh version =
    before_upgrade dbh;
    EzPG.upgrade ~dbh ~version ~downgrade upgrade;
    after_upgrade dbh;
  in
  cver := version;
  upgrades := !upgrades @ [previous_version, upgrade];
  downgrades := (version, downgrade) :: !downgrades;
  ()
;;

let init () =
  register_version ()
    (* list of instructions to execute to upgrade DB *)
    ~upgrade:[
      {|create table opam_index(
        opam_name varchar primary key,
        opam_version varchar not null,
        opam_synopsis varchar not null
      )|};
      {|create table library_index(
        lib_id int primary key,
        lib_name varchar not null,
        lib_opam varchar not null references opam_index(opam_name)
      )|};
      {|create table meta_index(
        meta_name varchar primary key,
        meta_opam varchar not null references opam_index(opam_name)
      )|};
      {|create table module_index(
        mdl_id int primary key,
        mdl_name varchar not null,
        mdl_path varchar not null,
        mdl_opam varchar not null references opam_index(opam_name)
      )|};
      {|create table module_libraries(
        mdl_id int not null references module_index(mdl_id),
        mdl_lib_id int not null references library_index(lib_id),
        mdl_lib varchar not null, 
        primary key (mdl_id, mdl_lib_id)
      )|};
      {|create table module_vals(
        mdl_id int not null references module_index(mdl_id),
        mdl_name varchar not null,
        mdl_opam_name varchar not null references opam_index(opam_name),
        mdl_ident varchar not null,
        mdl_val varchar not null, 
        primary key (mdl_id, mdl_ident)
      )|}
    ]
    (* list of instructions to execute to downgrade DB *)
    ~downgrade:[
      {|ALTER TABLE library_index DROP CONSTRAINT library_index_lib_opam_fkey|};
      {|ALTER TABLE meta_index DROP CONSTRAINT meta_index_meta_opam_fkey|};
      {|ALTER TABLE module_index DROP CONSTRAINT module_index_mdl_opam_fkey|};
      {|ALTER TABLE module_libraries DROP CONSTRAINT module_libraries_mdl_id_fkey|};
      {|ALTER TABLE module_libraries DROP CONSTRAINT module_libraries_mdl_lib_id_fkey|};
      {|ALTER TABLE module_vals DROP CONSTRAINT module_vals_mdl_id_fkey|};
      {|ALTER TABLE module_vals DROP CONSTRAINT module_vals_mdl_opam_name_fkey|};
      {|drop table opam_index|};
      {|drop table library_index|};
      {|drop table meta_index|};
      {|drop table module_index|};
      {|drop table module_libraries|};
      {|drop table module_vals|}
    ]
;;

let () = init ()
