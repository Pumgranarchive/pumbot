(*
** PumBot API Services
** This module registre services of the API
*)

module Yojson = Yojson.Basic
module Conf = Conf.Configuration

(** Save past launch uris  *)
let known_uris = ref []

(** Save current running uris  *)
let running_uris = ref []

let equal_uri u1 u2 = Ptype.compare_uri u1 u2 == 0

(** Add given uris only if there are totaly unkown *)
let filter uris =
  let aux uri =

    let exists = List.exists (equal_uri uri) !known_uris in

    (* If unknown, add to the list to not crawl again *)
    if not exists then known_uris := !known_uris@[uri];

    (* Limit the list size to 100 to avoid over flow *)
    if List.length !known_uris > 100 then known_uris := (List.tl !known_uris);

    not exists

  in
  List.filter aux uris

(* make command settings *)
let bin = Conf.Bot.directory ^"/pum_bot"
let option_a = ["-a"; Conf.Api.host]

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
  let forward = `FD_move fd in
  lwt status = Lwt_process.exec ~stdout:forward ~stderr:forward command in
  let () = Unix.close fd in

  Lwt.return status

(* Static settings for the prepare_and_exec function *)
let string_not_equal s1 s2 = String.compare s1 s2 != 0
let string_of_uri uri = "\"" ^ Ptype.string_of_uri uri ^ "\""

(** Launch the bot on the given uris *)
let prepare_and_exec max_deep not_recursice uris =

  (* Prepare options *)
  let uris = filter uris in
  let str_uris = List.map string_of_uri uris in
  let option_n = if not_recursice then ["-n"] else [] in
  let option_d = ["-d"; string_of_int max_deep] in
  let options = option_d @ option_n @ option_a @ str_uris in
  let command = make_command options in

  (* Generate logfile_name *)
  let first_str_uri = Ptype.string_of_uri (List.hd uris) in
  let uri_file = filename_of_uri first_str_uri in
  let logfile = Conf.Bot.logdir ^ uri_file ^".log" in

  (* Add uri to list *)
  running_uris := first_str_uri::!running_uris;

  (* Exec command *)
  lwt status = exec command logfile in

  (* Remove uri to list *)
  running_uris := List.filter (string_not_equal first_str_uri) !running_uris;

  Lwt.return status

let overloaded () =
  List.length !running_uris >= Conf.Bot.max_simultaneous_process

(** Run API function  *)
let run (max_deep, (not_recursice_opt, uris_encoded)) () =
  let not_recursice = match not_recursice_opt with
    | Some x -> x
    | None -> false
  in
  let uris_decoded = List.map Ptype.uri_decode uris_encoded in
  let uris = List.map Ptype.uri_of_string uris_decoded in
  let launch () = prepare_and_exec max_deep not_recursice uris in
  let overloading = overloaded () in
  if (List.length uris > 0) then (
    let first_str_uri = List.hd uris_decoded in
    if (not overloading) then Lwt.async launch
    else print_endline ("System overlading, thus not launching on "^ first_str_uri));
  let code = if (overloading) then 503 else 200 in
  let json = `Assoc [("code", `Int code)] in
  Lwt.return (Yojson.to_string json, "application/json")

(** Run API service  *)
let run_service =
  Eliom_service.Http.service
    ~path:["run"]
    ~get_params:Eliom_parameter.(suffix (int "max_deep" **
                                         opt (bool "not_recursive") **
                                         list "uris" (string "uri")))
    ()

(** Register run function  *)
let _ = Eliom_registration.String.register ~service:run_service run
