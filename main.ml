open Utils
open ArgParser

module UriMap = Map.Make(Uri)
module LinkMap = Map.Make(Link)

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
let is_equal a b _ = (Ptype.compare_uri a b = 0)

let push old queue uri =
  if (not (UriMap.exists (is_equal uri) old) &&
      Magic_queue.for_all (not_equal uri) queue)
  then Magic_queue.push uri queue
  else queue

let push_new_uris options list queue old =
  if not options.not_recursive
  then List.fold_left (push old) queue list
  else queue

let regeps = Str.regexps ["film"; "actor"; "actress"; "author"; "director"]

let is_authorized_tag tag_uri =
  let str = Ptype.string_of_uri tag_uri in
  Str.exists regeps str

let insert_tags uri new_tags uris =
  if List.exists is_authorized_tag new_tags
  then Lwt.async (fun () -> Tag.Of_Content.assign new_tags uri);
  UriMap.add uri new_tags uris

let insert_links uri new_links links uris =
  let add links link =
    let link_id = Ptype.link_id (Link.origin link) (Link.target link) in
    let link' =
      try Link.add_tags (LinkMap.find link_id links) (Link.tags link)
      with Not_found -> link
    in
    LinkMap.add link_id link' links
  in
  let build_insert_list link_id link link_list =
    try
      let second_uri =
        if Uri.compare (Link.origin link) uri = 0 then Link.target link
        else if Uri.compare (Link.target link) uri = 0 then Link.origin link
        else raise Not_found
      in
      let second_tags = UriMap.find second_uri uris in
      if List.exists is_authorized_tag second_tags
      then link::link_list
      else link_list
    with Not_found -> link_list
  in
  let links' = List.fold_left add links new_links in
  let tags = UriMap.find uri uris in
  let () = if (List.exists is_authorized_tag tags) then
      let to_insert = LinkMap.fold build_insert_list links' [] in
      Lwt.async (fun () -> Link.insert to_insert)
  in
  links'

let iter switch options uris =
  let aux (uris, links) queue uri =
    lwt new_tags, new_links, new_uris = switch uri in
    let uris' = insert_tags uri new_tags uris in
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
  let ilist = List.tl (Array.to_list Sys.argv) in
  try run (ArgParser.get_options ilist)
  with
  | ArgParser.Invalid_Argument
  | ArgParser.Help             -> Lwt.return ()

lwt () = main ()
