open Lwt.Infix
open Data_types

let to_api p = Lwt.bind p EzAPIServerUtils.return

let version _params () = to_api (
    Db.get_version () >|= fun v_db_version ->
    Ok { v_db = PConfig.database; v_db_version })

let generate _params () = to_api (
    Index.generate () >|= fun generated ->
    Ok { generated })