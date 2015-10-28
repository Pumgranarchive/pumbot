open Utils
open ArgParser

module UriMap = Map.Make(Uri)
module LinkMap = Map.Make(LinkId)

let platforms =
  [(Youtube.is_youtube_uri,         Youtube.switch);
   (Dbpedia.is_wikipedia_uri,       Dbpedia.get);
   (ExtractTools.is_something_else, Pxtractor.get)]

let rec switch platforms uri =
  match platforms with
  | (condiction, action)::next ->
    if condiction uri
    then action uri
    else switch next uri
  | [] -> raise Not_found

let not_equal a b = (Ptype.compare_uri a b != 0)
let is_equal a b _ = (Ptype.compare_uri a b = 0)

(**
   Push new_uri into the queue
   If it is not already contains into old list or the queue itself
*)
let push old queue uri =
  if (UriMap.for_all (fun k v -> not_equal uri k) old &&
      Magic_queue.for_all (not_equal uri) queue)
  then Magic_queue.push uri queue
  else queue

let push_new_uris options new_uris queue old =
  if not options.not_recursive             (* If recursive mode is activated *)
  then List.fold_left (push old) queue new_uris
  else queue



(**
   Insert the content if it contains at least one authorized subject
   Then return new known uris list with the associated subjects
*)
let insert_content content uris =
  let authorized = Content.is_authorized content in
  if authorized then Lwt.async (fun () -> Content.insert content);
  UriMap.add (Content.uri content) authorized uris

(**
   - Add new links into the old links if they are not already inside
   - Insert new links if the origin and target content is authorized

   Then return new known links list
*)
let insert_links uri new_links links uris =
  let add_if_not_exists links link =
    let link_id = Link.id link in
    if LinkMap.for_all (fun id link -> String.compare id link_id != 0) links
    then LinkMap.add link_id link links
    else links
  in
  let filter_to_insert link_id link link_list =
    try
      let second_uri =
        if Uri.compare (Link.origin link) uri = 0 then Link.target link
        else if Uri.compare (Link.target link) uri = 0 then Link.origin link
        else raise Not_found
      in
      let authorized = UriMap.find second_uri uris in
      if authorized then link::link_list else link_list
    with Not_found -> link_list
  in
  let links' = List.fold_left add_if_not_exists links new_links in
  let authorized = UriMap.find uri uris in
  let () = if authorized then
      let to_insert = LinkMap.fold filter_to_insert links' [] in
      Lwt.async (fun () -> Link.insert to_insert)
  in
  links'

(**
   Get information from content,
   - Insert them
   - Build new lists of known uris and links
   - Add new uris to parse

   Until there is no uri left in the queue to visit.
   Or the limit of iterations
*)
let iter switch options uris =
  let aux (uris, links) queue uri =
    lwt content, new_links, new_uris = switch uri in
    let uris' = insert_content content uris in
    let links' = insert_links uri new_links links uris' in
    let queue' = push_new_uris options new_uris queue uris' in
    Lwt.return ((uris', links'), queue')
  in
  let queue = Magic_queue.from_list uris in
  let empty = (UriMap.empty, LinkMap.empty) in
  lwt _ = Lwt_magic_queue.fold_left aux options.iteration_max empty queue in
  Lwt.return ()

let run (list, options) =
  let uris = List.map Ptype.uri_of_string list in
  iter (switch platforms) options uris

let main () =
  let input_list = List.tl (Array.to_list Sys.argv) in
  try run (ArgParser.get_options input_list)
  with
  | ArgParser.Invalid_Argument
  | ArgParser.Help             -> Lwt.return ()

lwt () = main ()
