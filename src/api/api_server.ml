
(** Entry module for Index API that starts and configurates the server *)

let api_port = ref PConfig.api_port
(** API port, loaded from config module *)

let load_config filename =
  try
    let ic = open_in filename in
    let json = Ezjsonm.from_channel ic in
    close_in ic ;
    let port = Json_encoding.destruct Encoding.api_config json in
    (match port with None -> () | Some port -> api_port := port);
  with _ -> Printf.eprintf "Fatal error: cannot parse config file %S\n%!" filename
(** Loads configuration file and initialise server variables *)

let catch path exn =
  EzAPIServer.Answer.(return ~headers ~code:500 @@
  EzEncoding.construct Json_encoding.(obj1 (req "error" string)) @@ path ^ ": " ^ Printexc.to_string exn)

let server services =
  Printexc.record_backtrace true;
  (* Load config file *)
  Arg.parse [] (fun config_file ->
      load_config config_file) "API server" ;
  let servers = [ !api_port, EzAPIServerUtils.API services ] in
  Lwt_main.run (
    (* Prints server's port *)
    Printf.eprintf "Starting servers on ports [%s]\n%!"
      (String.concat ","
         (List.map (fun (port,_) ->
              string_of_int port) servers));
    (* Launch server *)  
    EzAPIServer.server ~catch servers
  )
(** Launch API server with specified services of type [EzAPIServerUtils.Directory.t]. *)

let () =
  server Api.services
