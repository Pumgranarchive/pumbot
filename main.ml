open Utils

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

let iter switch uris =
  let queue = Magic_queue.from_list uris in
  let not_equal a b = (Ptype.compare_uri a b != 0) in
  let push old queue uri =
    if (List.for_all (not_equal uri) old &&
        Magic_queue.for_all (not_equal uri) queue)
    then Magic_queue.push uri queue
    else queue
  in
  let push_list list queue old = List.fold_left (push old) queue list in
  let aux old queue uri =
    lwt to_add = switch uri in
    let old' = uri::old in
    let queue' = push_list to_add queue old' in
    Lwt.return (old', queue')
  in
  lwt old = Lwt_magic_queue.fold_left aux [] queue in
  Lwt.return ()

let main () =
  let input_list = List.tl (Array.to_list Sys.argv) in
  let uris = List.map Ptype.uri_of_string input_list in
  lwt () = iter (switch platforms) uris in
  Lwt.return ()

lwt () = main ()
