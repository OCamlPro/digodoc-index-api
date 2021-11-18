(** Entry module for db_updater executable. *)

let () =
  EzPGUpdater.main PConfig.database
    ~downgrades:Versions.(!downgrades)
    ~upgrades:Versions.(!upgrades)
