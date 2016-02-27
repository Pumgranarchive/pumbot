(** Main
 * This module initialize the bot
 *)

(** Initialize the pumgrana api uri *)
let init options =
  Pumgrana_http.set_pumgrana_api_uri options.ArgParser.api_host

let header () =
  print_endline "";
  print_endline "#############################################################";
  print_endline "####################### Start Crawling ######################";
  print_endline "#############################################################";
  print_endline ""

let main () =
  let () = header () in
  let input_list = List.tl (Array.to_list Sys.argv) in
  let uri_list, options = ArgParser.get_options input_list in
  let () = init options in
  try Crawler.start uri_list options
  with
  | ArgParser.Invalid_Argument
  | ArgParser.Help             -> Lwt.return ()

lwt () = main ()
