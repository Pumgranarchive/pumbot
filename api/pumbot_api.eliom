(*
** PumBot API Services
** This module registre services of the API
*)

module Yojson = Yojson.Basic

(** Save past launch uris  *)
let old = ref []

(** Add given uris only if there are totaly unkown *)
let filter_and_add uris =
  let aux uri =
    let exists = List.exists (fun x -> Ptype.compare_uri uri x = 0) !old in
    if not exists then old := uri::!old;
    not exists
  in
  List.filter aux uris

(** Launch the bot on the given uris *)
let launch max_deep not_recursice uris =
  let uris = filter_and_add uris in
  let path = "../botapp/pum_bot" in
  let option_n = if not_recursice then " -n" else "" in
  let option_d = " -d " ^ (string_of_int max_deep) in
  let options = option_d ^ option_n in
  let redirect = "> /dev/null 2>&1" in
  let bg = "&" in
  let string_of_uri uri = "\"" ^ Ptype.string_of_uri uri ^ "\"" in
  let str_uris = List.map string_of_uri uris in
  let concat_uris = String.concat " " str_uris in
  let cmd = String.concat " " [path; options; concat_uris; (* redirect; *) bg] in
  if List.length uris > 0
  then Lwt.async (fun () -> print_endline cmd; Lwt.return (ignore (Sys.command cmd)))

(** Run API function  *)
let run (max_deep, (not_recursice_opt, uris_encode)) () =
  let not_recursice = match not_recursice_opt with
    | Some x -> x
    | None -> false
  in
  let uris = List.map (fun x -> Ptype.uri_of_string (Ptype.uri_decode x)) uris_encode in
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
