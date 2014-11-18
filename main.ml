open Utils
open ArgParser

let platforms =
  [(Youtube.is_youtube_uri,         Youtube.switch);
   (Dbpedia.is_wikipedia_uri,       Dbpedia.get);
   (Readability.is_something_else,  Readability.get)]

let rec switch platforms uri =
  match platforms with
  | (condiction, action)::next ->
    if condiction uri
    then action uri
    else switch next uri
  | [] -> raise Not_found

let not_equal a b = (Ptype.compare_uri a b != 0)

let push old queue uri =
  if (List.for_all (not_equal uri) old &&
        Magic_queue.for_all (not_equal uri) queue)
  then Magic_queue.push uri queue
  else queue

let push_list list queue old = List.fold_left (push old) queue list

let iter switch options uris =
  let aux old queue uri =
    lwt to_add = switch uri in
    let old' = uri::old in
    let queue' =
      if options.not_recursive
      then queue
      else push_list to_add queue old'
    in
    Lwt.return (old', queue')
  in
  let queue = Magic_queue.from_list uris in
  lwt old = Lwt_magic_queue.fold_left aux options.iteration_max [] queue in
  Lwt.return ()

let run list options =
  let uris = List.map Ptype.uri_of_string list in
  iter (switch platforms) options uris

let main () =
  let ilist = List.tl (Array.to_list Sys.argv) in
  try
    let list, options = ArgParser.get_options ilist in
    run list options
  with
    ArgParser.Invalid_Argument
  | ArgParser.Help             -> Lwt.return ()

lwt () = main ()
