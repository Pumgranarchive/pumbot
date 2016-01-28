(*
** PumBot API Services
** This module registre services of the API
*)

module Yojson = Yojson.Basic
module Conf = Conf.Configuration

(** Save past launch uris  *)
let known_uris = ref []

let equal u1 u2 = Ptype.compare_uri u1 u2 == 0

(** Add given uris only if there are totaly unkown *)
let filter uris =
  let aux uri =

    let exists = List.exists (equal uri) !known_uris in

    (* If unknown, add to the list to not crawl again *)
    if not exists then known_uris := !known_uris@[uri];

    (* Limit the list size to 100 to avoid over flow *)
    if List.length !known_uris > 100 then known_uris := (List.tl !known_uris);

    not exists

  in
  List.filter aux uris

(* Static settings for the launch function *)
let bg = "&"
let bin = "./pum_bot"
let cd = "cd " ^ Conf.Bot.directory
let string_of_uri uri = "\"" ^ Ptype.string_of_uri uri ^ "\""
let not_alphanum = Str.regexp "[^a-zA-Z\d\s]"

(** Launch the bot on the given uris *)
let launch max_deep not_recursice uris =
  let uris = filter uris in
  let option_n = if not_recursice then " -n" else "" in
  let option_d = " -d " ^ (string_of_int max_deep) in
  let options = option_d ^ option_n in
  let first_uri = Ptype.string_of_uri (List.hd uris) in
  let uri_file = Str.global_replace not_alphanum "_" first_uri in
  let logfile = Conf.Bot.logdir ^ uri_file ^".log" in
  let redirect = ">> "^ logfile ^" 2>&1" in
  let str_uris = List.map string_of_uri uris in
  let concat_uris = String.concat " " str_uris in
  let cmd = String.concat " " [cd; "&&"; bin; options; concat_uris; redirect; bg] in
  if List.length uris > 0
  then Lwt.async (fun () -> print_endline cmd; Lwt.return (ignore (Sys.command cmd)))

(** Run API function  *)
let run (max_deep, (not_recursice_opt, uris_encode)) () =
  let not_recursice = match not_recursice_opt with
    | Some x -> x
    | None -> false
  in
  let uris = List.map (fun x -> Ptype.uri_of_string (Ptype.uri_decode x)) uris_encode in
  print_endline "Run on ::";
  List.iter (fun u -> print_endline (Ptype.string_of_uri u)) uris;
  print_endline "";
  let () = launch max_deep not_recursice uris in
  let json = `Assoc [("code", `Int 200)] in
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
