val get_sys_argv : unit -> string
val safe_setup_logger : ?output_mode:string -> unit -> unit Lwt.t
val safe_create_socket : ?mode:string -> unit -> Lwt_unix.file_descr option Lwt.t
val safe_create_channels: Lwt_unix.file_descr -> (Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel) option Lwt.t
