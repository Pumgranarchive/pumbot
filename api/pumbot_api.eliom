(*
** PumBot API Services
** This module registre services of the API
*)

module Yojson = Yojson.Basic
module Conf = Conf.Configuration

(** Waiting list  *)
let waiting_uris = Queue.create ()
let waiting_size = 100

(** Save past launch uris  *)
let known_uris = Queue.create ()
let known_size = 300

(** Save current running uris  *)
let running_uris = ref []

let equal_uri u1 u2 = Ptype.compare_uri u1 u2 == 0

let queue_exists f = Queue.fold (fun exist elm -> exist || f elm) false

let queue_pop q = ignore (Queue.pop q)

(** Add given uris only if there are totaly unkown *)
let filter uris =
  let aux uri =

    let exists = queue_exists (equal_uri uri) known_uris in

    (* If unknown, add to the list to not crawl again *)
    if not exists then Queue.push uri known_uris;

    (* Limit the queue size to [known_size] to avoid over flow *)
    if Queue.length known_uris > known_size then queue_pop known_uris;

    not exists

  in
  List.filter aux uris

(* make command settings *)
let bin = Conf.Bot.directory ^"/pum_bot"
let option_a = ["-a"; Conf.Api.host]


let push_waiting uri =
  if (Queue.length waiting_uris > waiting_size)
  then Queue.push uri waiting_uris


let make_command options =
  (bin, Array.of_list (bin::options))

let print_cmd (bin, command) logfile =
  let cmd_str = Array.fold_left (fun str opt -> str ^" "^ opt) "" command in
  let with_redirection = cmd_str ^" 2>1 "^ logfile in
  print_endline with_redirection

(* Static settings for filename_of_uri function *)
let not_alphanum = Str.regexp "[^a-zA-Z0-9\\s]"
let http = Str.regexp "^https?://"

let filename_of_uri str_uri =
  let without_protocol = Str.global_replace http "" str_uri in
  Str.global_replace not_alphanum "_" without_protocol

(* Exec settings *)
let open_log_flag = Unix.([O_WRONLY; O_APPEND; O_CREAT])
let log_permition = 0o440               (* Read for user and group *)

let exec command logfile =

  (* Print command *)
  print_cmd command logfile;

  let fd = Unix.openfile logfile open_log_flag log_permition in
  let forward = `FD_copy fd in
  lwt status = Lwt_process.exec ~stdout:forward ~stderr:forward command in
  let () = Unix.close fd in

  Lwt.return status

(* Static settings for the prepare_and_exec function *)
let string_not_equal s1 s2 = String.compare s1 s2 != 0

(** Launch the bot on the given uris *)
let rec prepare_and_exec max_deep not_recursive uris =

  (* Prepare options *)
  let str_uris = List.map Ptype.string_of_uri uris in
  let option_n = if not_recursive then ["-n"] else [] in
  let option_d = ["-d"; string_of_int max_deep] in
  let options = option_d @ option_n @ option_a @ str_uris in
  let command = make_command options in

  (* Generate logfile_name *)
  let first_str_uri = Ptype.string_of_uri (List.hd uris) in
  let uri_file = filename_of_uri first_str_uri in
  let logfile = Conf.Bot.logdir ^ uri_file ^ ".log" in

  (* Add uri to list *)
  running_uris := first_str_uri::!running_uris;

  (* Exec command *)
  lwt status = exec command logfile in

  (* Remove uri to list *)
  running_uris := List.filter (string_not_equal first_str_uri) !running_uris;

  (* Launch one waiting uri if possible *)
  if Queue.length waiting_uris > 0 && List.length !running_uris < Conf.Bot.min_process
  then Lwt.async (fun () -> prepare_and_exec max_deep not_recursive [Queue.pop waiting_uris]);

  Lwt.return status

let overloaded () =
  List.length !running_uris >= Conf.Bot.max_process

(** Mannage lists and launch *)
let manager max_deep not_recursive unfiltered_uris =
  let uris = filter unfiltered_uris in
  let launch () = prepare_and_exec max_deep not_recursive uris in
  let overloading = overloaded () in
  if (List.length uris > 0) then (
    let first_uri = List.hd uris in
    if (overloading) then push_waiting first_uri
    else Lwt.async launch );
  if (overloading) then 503 else 200

(** Controller receiving api calls *)
let controller (max_deep, (not_recursive_opt, uris_encoded)) () =
  let not_recursive = match not_recursive_opt with
    | Some x -> x
    | None -> false
  in
  let uris_decoded = List.map Ptype.uri_decode uris_encoded in
  let uris = List.map Ptype.uri_of_string uris_decoded in
  let code = manager max_deep not_recursive uris in
  let json = `Assoc [("code", `Int code)] in
  Lwt.return (Yojson.to_string json, "application/json")

(** API service description *)
let run_service =
  Eliom_service.Http.service
    ~path:["run"]
    ~get_params:Eliom_parameter.(suffix (int "max_deep" **
                                         opt (bool "not_recursive") **
                                         list "uris" (string "uri")))
    ()

(** Register controller to service  *)
let _ = Eliom_registration.String.register ~service:run_service controller
