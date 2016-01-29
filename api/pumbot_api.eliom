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

(* Static settings for the launch function *)
let bin = "./pum_bot"
let cd = "cd " ^ Conf.Bot.directory
let string_of_uri uri = "\"" ^ Ptype.string_of_uri uri ^ "\""
let not_alphanum = Str.regexp "[^a-zA-Z\d\s]"
let string_not_equal s1 s2 = String.compare s1 s2 != 0

(** Launch the bot on the given uris *)
let launch max_deep not_recursice uris =
  let uris = filter uris in
  let option_n = if not_recursice then " -n" else "" in
  let option_d = " -d " ^ (string_of_int max_deep) in
  let options = option_d ^ option_n in
  let first_str_uri = Ptype.string_of_uri (List.hd uris) in
  let uri_file = Str.global_replace not_alphanum "_" first_str_uri in
  let logfile = Conf.Bot.logdir ^ uri_file ^".log" in
  let redirect = ">> "^ logfile ^" 2>&1" in
  let str_uris = List.map string_of_uri uris in
  let concat_uris = String.concat " " str_uris in
  let cmd = String.concat " " [cd; "&&"; bin; options; concat_uris; redirect] in
  (* Log launched command *)
  print_endline cmd;
  (* Add uri to list *)
  running_uris := first_str_uri::!running_uris;
  (* Launch command *)
  lwt exit_code = Lwt.return (Sys.command cmd) in
  (* Remove uri to list *)
  running_uris := List.filter (string_not_equal first_str_uri) !running_uris;
  (* Return exit code *)
  Lwt.return exit_code

let overloaded () =
  List.length !running_uris >= Conf.Bot.max_simultaneous_process

(** Run API function  *)
let run (max_deep, (not_recursice_opt, uris_encoded)) () =
  let not_recursice = match not_recursice_opt with
    | Some x -> x
    | None -> false
  in
  let uris_decoded = List.map Ptype.uri_decode uris_encoded in
  let first_str_uri = List.hd uris_decoded in
  let uris = List.map Ptype.uri_of_string uris_decoded in
  let launch () = launch max_deep not_recursice uris in
  let overloading = overloaded () in
  if (overloading)
  then print_endline ("System overlading, thus not launching on "^ first_str_uri);
  if (List.length uris > 0 && not overloading) then Lwt.async launch;
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
