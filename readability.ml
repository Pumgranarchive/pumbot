open Utils

module Yojson = Yojson.Basic

(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

let get_token () =
  let ic = open_in "token" in
  try
    let token = input_line ic in
    let () = close_in ic in
    token
  with e ->
    close_in_noerr ic;
    raise Not_found

let () = Readability_http.set_token (get_token ())
let () = Opencalais_http.set_token "z3k9bmug6udbqcqdwgt8qzq2"

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let get_contained_uris body =
  let ink = Str.regexp "#.*" in
  let cand = Str.regexp "&.*" in
  let clean uri =
    Str.global_replace cand ""
      (Str.global_replace ink "" uri)
  in

  let wiki_action = Str.regexp "^https?://..\\.wikipedia\\.org/w/index\\.php" in
  let img = Str.regexp ".*\\.\\(jpg\\|png\\|svg\\|bmp\\|gif\\)$" in
  let wiki_file = Str.regexp ".*file:.*" in
  let wiki_category = Str.regexp ".*category:.*" in
  let wiki_special = Str.regexp ".*special:.*" in
  let wiki_help = Str.regexp ".*help:.*" in
  let should_be_removed uri =
    Str.string_partial_match wiki_action uri 0 ||
    Str.string_match wiki_file uri 0 ||
    Str.string_match wiki_category uri 0 ||
    Str.string_match wiki_special uri 0 ||
    Str.string_match wiki_help uri 0 ||
    Str.string_match img uri 0
  in

  let compare s1 s2 = String.compare s1 (String.lowercase s2) == 0 in

  let uri_expr = Str.regexp "[\"']https?://.*[\"']" in
  let end_uri = Str.regexp "[\"']" in
  let rec extract_uri uris pos =
    try
      let start_p = Str.search_forward uri_expr body pos in
      let end_p = Str.search_forward end_uri body (start_p + 1) in
      let uri = clean (String.sub body (start_p + 1) (end_p - start_p - 1)) in
      let lower_uri = String.lowercase uri in
      if (should_be_removed lower_uri || List.exists (compare lower_uri) uris)
      then extract_uri uris (end_p + 1)
      else extract_uri (uri::uris) (end_p + 1)
    with Not_found -> uris
  in
  let str_uris = extract_uri [] 0 in
  let uris = List.rev (List.map Ptype.uri_of_string str_uris) in
  List.limit 40 uris

let get_body uri =
  lwt json = Readability_http.get_parser uri in
  let body = Yojson.Util.(to_string (member "content" json)) in
  Tidy.xhtml_of_html body

let get_data uri =
  lwt json = Readability_http.get_parser uri in
  let title = Yojson.Util.(to_string (member "title" json)) in
  let body = Yojson.Util.(to_string (member "content" json)) in
  lwt body' = Tidy.xhtml_of_html body in
  Lwt.return (title, body')

let get_social_tags json =
  let open Yojson.Util in
  let aux blist (title, elm) =
    let type_group = member "_typeGroup" elm in
    if (type_group != `Null &&
        String.compare (to_string type_group) "socialTag" == 0)
    then
      let name = to_string (member "name" elm) in
      let name' = Str.global_replace (Str.regexp "[_\\.]") " " name in
      name'::blist
    else blist
  in
  let list = to_assoc json in
  List.fold_left aux [] list

lwt talk_about = Tag.Of_Link.make "Talk about"
lwt mentioned_by = Tag.Of_Link.make "Mentioned by"

(******************************************************************************
******************************** Functions ************************************
*******************************************************************************)

let is_something_else uri = true

let get uri =
  print_endline "Readability";
  lwt title, body = get_data uri in
  let buris = get_contained_uris body in
  lwt yuris = Youtube.search title in
  let uris = yuris@buris in
  lwt json = Opencalais_http.request ~display_body:false body in
  let subjects = get_social_tags json in
  lwt tag_ids = Tag.Of_Content.makes subjects in
  let links = Link.build_inter_link [talk_about] [mentioned_by] [uri] uris in
  Lwt.return (tag_ids, links, uris)
