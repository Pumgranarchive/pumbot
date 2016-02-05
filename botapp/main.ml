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
let push old deep queue uri =
  if (UriMap.for_all (fun k v -> not_equal uri k) old &&
      Magic_queue.for_all (fun (d, u) -> not_equal uri u) queue)
  then Magic_queue.push (deep, uri) queue
  else queue

let push_new_uris options deep new_uris old queue =
  if not options.not_recursive             (* If recursive mode is activated *)
  then List.fold_left (push old deep) queue new_uris
  else queue



(**
   Insert the content if it contains at least one authorized subject
   Then return new known uris list with the associated subjects
*)
let insert_content content uris =
  let authorized = Content.is_authorized content in
  lwt _ = if authorized then Content.insert content else Lwt.return None in
  Lwt.return (UriMap.add (Content.uri content) authorized uris)

(**
   - Add new links into the old links if they are not already inside
   - Insert new links if the origin and target content is authorized

   Then return new known links list
*)
let old_insert_links uri new_links links uris =
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
   insert all links
*)
let insert_links new_links = Link.insert new_links

(**
   Get information from content,
   - Insert them
   - Build new lists of known uris and links
   - Add new uris to parse

   Until there is no uri left in the queue to visit.
   Or the limit of iterations
*)
let iter switch options queue =
  let aux old_uri_list queue (deep, uri) =
    lwt opt_data = switch uri in
    match opt_data with

    (* Not any data found - Invalid url *)
    | None -> Lwt.return (old_uri_list, queue)

    (* Some data have been extracted *)
    | Some (content, new_links, new_uris) ->
      lwt uri_list = insert_content content old_uri_list in
      lwt _ = insert_links new_links in
      let new_deep = deep + 1 in
      let new_queue = push_new_uris options new_deep new_uris uri_list queue in
      Lwt.return (uri_list, new_queue)
  in
  lwt _ = Lwt_magic_queue.fold_left aux options UriMap.empty queue in
  Lwt.return ()

let run uri_list options =
  let init str = (0, Ptype.uri_of_string str) in
  let uri_list' = List.map init uri_list in
  let queue = Magic_queue.from_list uri_list' in
  iter (switch platforms) options queue

(** Initialize the pumgrana api uri *)
let init options =
  Pumgrana_http.set_pumgrana_api_uri options.api_host

let main () =
  let input_list = List.tl (Array.to_list Sys.argv) in
  let uri_list, options = ArgParser.get_options input_list in
  let () = init options in
  try run uri_list options
  with
  | ArgParser.Invalid_Argument
  | ArgParser.Help             -> Lwt.return ()

lwt () = main ()
