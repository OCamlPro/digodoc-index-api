(** Entry module for openapi executable, that creates API spec documentation *)

let () =
  let sections = Services.sections in
  EzOpenAPI.executable ~sections ~docs:[]
