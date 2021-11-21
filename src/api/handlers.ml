open Lwt.Infix
open Data_types

(** Module that defines behaviour for every service from [Services] module. *)

let to_api p = Lwt.bind p EzAPIServerUtils.return
(** Encapsulates promise value to answer [EzAPIServerUtils.Answer.t] *)

let generate _params () = to_api (
    Index.generate () >|= fun generated ->
    Ok { generated })
(** Handler for [Services.generate] service. Looks up for content in directory
    [Index.docs_dir] and fills Digodoc DB. *)

let sources _params () = to_api (
    Index.sources () >|= fun generated ->
    Ok { generated })
(** Handler for [Services.sources] service. Looks up for content in directory
    [Index.sources_dir] and indaxate it in [PConfig.digodoc_dir]/sources_db . *)