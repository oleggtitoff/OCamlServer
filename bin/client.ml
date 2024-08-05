let time_alive () =
  let random_time = 60 + Random.int (120 - 60 + 1) in
  Lwt_unix.sleep (float_of_int random_time)

let read_line ic oc id =
  let%lwt msg = Lwt_io.read_line_opt ic in
  match msg with
  | Some msg ->
      let reply = "Received:" ^ id ^ ":" ^ (string_of_int @@ String.length msg) in
      Lwt_io.write_line oc reply
  | None ->
      Lwt_log.info ("[read_line] id: " ^ id ^ " message is empty")

let rec handle_connection ic oc id =
  let%lwt () = read_line ic oc id in
  handle_connection ic oc id

let safe_handle_connection ic oc id =
  try%lwt
    handle_connection ic oc id
  with exn ->
    Lwt_log.warning ("[handle_connection] id: " ^ id ^ " " ^ Printexc.to_string exn)

let rec send_alive oc id =
  let sleep_time = 5 + Random.int (20 - 5 + 1) in
  let%lwt () = Lwt_unix.sleep (float_of_int sleep_time) in
  let%lwt () = Lwt_io.write_line oc ("I'm alive:" ^ id) in
  send_alive oc id

let safe_send_alive oc id =
  try%lwt
    send_alive oc id
  with exn ->
    Lwt_log.warning ("[send_alive] id: " ^ id ^ " " ^ Printexc.to_string exn)

let () =
  Random.self_init ();
  let id = Routine.get_sys_argv () in

  Lwt_main.run begin
    let%lwt () = Routine.safe_setup_logger ~output_mode:"stdout" () in
    let%lwt socket_result = Routine.safe_create_socket ~mode:"client" () in
    match socket_result with
    | Some socket ->
        let%lwt channels_result = Routine.safe_create_channels socket in
        let%lwt () =
          match channels_result with
          | Some (ic, oc) ->
                Lwt.pick [
                  time_alive ();
                  safe_handle_connection ic oc id;
                  safe_send_alive oc id
                ]
          | None -> Lwt_log.error ("[safe_create_channels] id: " ^ id ^ " Failed to create channels") in
        Lwt_unix.close socket
    | None -> Lwt_log.error ("[safe_create_socket] id: " ^ id ^ " Failed to create socket")
  end
