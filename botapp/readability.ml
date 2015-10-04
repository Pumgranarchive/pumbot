open Utils

module Yojson = Yojson.Basic

(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

let () = Readability_http.set_token Token.readability
let () = Opencalais_http.set_token Token.opencalais

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
  let summary = Yojson.Util.(to_string (member "excerpt" json)) in
  let body = Yojson.Util.(to_string (member "content" json)) in
  (* lwt body' = Tidy.xhtml_of_html body in *)
  Lwt.return (title, summary, body)

let talk_about = "Talk about"
let mentioned_by = "Mentioned by"

(******************************************************************************
******************************** Functions ************************************
*******************************************************************************)

let is_something_else uri = true

let get uri =
  print_endline "Readability";

  lwt title, summary, body = get_data uri in
  lwt json = Opencalais_http.request body in
  let subjects = Opencalais_http.to_social_tags json in
  let tags = Tag.makes subjects in
  let content = Content.make uri title summary tags in

  let buris = get_contained_uris body in
  let rlinks = Link.build_inter_link talk_about mentioned_by [uri] buris in

  lwt yuris = Youtube.search title in
  let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
  let _ = Link.insert ylinks in (* Always insert youtube links  *)

  let uris = yuris@buris in

  Lwt.return (content, rlinks, uris)
