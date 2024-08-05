let clients_oc = ref []

let handle_message msg =
  let msg_list = String.split_on_char ':' msg in
  match msg_list with
  | ["Received"; id; length] -> "[Received] Client " ^ id ^ " received message of " ^ length ^ " symbols"
  | ["I'm alive"; id] -> "[Alive] Client " ^ id ^ " is alive"
  | _ -> "Unknown command"

let rec read_line ic =
  let%lwt msg = Lwt_io.read_line_opt ic in
  match msg with
  | Some msg ->
      let%lwt () = Lwt_log.info @@ handle_message msg in
      read_line ic
  | None ->
      Lwt_log.info "[read_line] message is empty"

let handle_connection file_descriptor =
  let%lwt channels_result = Routine.safe_create_channels file_descriptor in
  match channels_result with
  | Some (ic, oc) ->
    clients_oc := oc :: !clients_oc;
    Lwt.async (fun () -> read_line ic);
    Lwt.return_unit
  | None -> Lwt.return_unit

let rec accept_connection socket =
  let%lwt file_descriptor, _ = Lwt_unix.accept socket in
  let%lwt () = handle_connection file_descriptor in
  accept_connection socket

let safe_accept_connection socket =
  try%lwt
    accept_connection socket
  with exn ->
    Lwt_log.warning ("[accept_connection] " ^ Printexc.to_string exn)

let rec run_client id =
  let command = Lwt_process.shell ("./_build/default/bin/client.exe " ^ id) in
  let process = Lwt_process.open_process_none command in
  let%lwt status = process#status in
  match status with
  | Unix.WEXITED 0 ->
      let%lwt () = Lwt_log.info ("[Restarting] Client " ^ id ^ " finished with zero exit code. Restarting") in
      run_client id
  | Unix.WEXITED code | Unix.WSIGNALED code | Unix.WSTOPPED code ->
      Lwt_log.warning ("[run_client] Client " ^ id ^ " ended abnormally with code: " ^ string_of_int code)

let run_clients client_count =
  let client_ids = List.init client_count (fun i -> string_of_int (i + 1)) in
  Lwt_list.iter_p run_client client_ids

let safe_run_clients client_count =
  try%lwt
    run_clients client_count
  with exn ->
    Lwt_log.warning ("[run_clients] " ^ Printexc.to_string exn)

let rec handle_user_input () =
  let%lwt input = Lwt_io.read_line Lwt_io.stdin in
  let%lwt () = Lwt_list.iter_p (fun oc -> Lwt_io.write_line oc input) !clients_oc in
  handle_user_input ()

let safe_handle_user_input () =
  try%lwt
    handle_user_input ()
  with exn ->
    Lwt_log.warning ("[handle_user_input] " ^ Printexc.to_string exn)

let () =
  let client_count = int_of_string @@ Routine.get_sys_argv () in

  Lwt_main.run begin
    let%lwt () = Routine.safe_setup_logger () in
    let%lwt socket_result = Routine.safe_create_socket () in
    match socket_result with
    | Some socket ->
        let%lwt () =
          Lwt.pick [
            safe_accept_connection socket;
            safe_run_clients client_count;
            safe_handle_user_input ()
          ] in
        Lwt_unix.close socket
    | None -> Lwt_log.error ("[safe_create_socket] Failed to create socket")
  end
