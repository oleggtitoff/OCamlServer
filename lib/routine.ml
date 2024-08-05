let get_sys_argv () =
  if Array.length Sys.argv > 1 then Sys.argv.(1) else "0"


let open_file () = 
  let file_name = "log.txt" in
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] in
  let permitions = 0o640 in

  let%lwt file_descriptor = Lwt_unix.openfile file_name flags permitions in
  Lwt_unix.set_close_on_exec file_descriptor;
  Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.output file_descriptor)

let get_output_channel output_mode =
  match output_mode with
  | "stdout" -> Lwt.return Lwt_io.stdout
  | _ -> open_file ()

let setup_logger output_mode =
  let%lwt output_channel = get_output_channel output_mode in

  Lwt_log.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(level)] $(message)"
      ~close_mode:`Close
      ~channel:output_channel
      ();

  Lwt_log.add_rule "*" Lwt_log.Info;
  Lwt.return_unit

let safe_setup_logger ?(output_mode="file") () =
  try%lwt
    setup_logger output_mode
  with exn ->
    Lwt_log.warning ("[setup_logger] " ^ Printexc.to_string exn)


let create_socket mode =
  let address = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 54321 in
  let backlog = 10 in

  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let sockaddr = ADDR_INET (address, port) in

  let%lwt () =
    match mode with
    | "client" -> 
        connect sock sockaddr
    | _ ->
        let%lwt () = bind sock sockaddr in
        Lwt.return (listen sock backlog) in

  Lwt.return (Some sock)

let safe_create_socket ?(mode="server") () =
  try%lwt
    create_socket mode
  with exn ->
    let%lwt () = Lwt_log.warning ("[create_socket] " ^ Printexc.to_string exn) in
    Lwt.return_none


let create_channels file_descriptor =
  let input_channel = Lwt_io.of_fd ~mode:Lwt_io.Input file_descriptor in
  let output_channel = Lwt_io.of_fd ~mode:Lwt_io.Output file_descriptor in
  Lwt.return (Some (input_channel, output_channel))

let safe_create_channels file_descriptor =
  try%lwt
    create_channels file_descriptor
  with exn ->
    let%lwt () = Lwt_log.warning ("[create_channels] " ^ Printexc.to_string exn) in
    Lwt.return_none
