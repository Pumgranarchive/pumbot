open Utils

(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

let () = Opencalais_http.set_token Token.opencalais

(******************************************************************************
****************************** Extract Uris ***********************************
*******************************************************************************)

let is_something_else uri = true

let contained_uris_of body =
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

(******************************************************************************
***************************** Extract html meta *******************************
*******************************************************************************)

type meta = { title: string; description: string }

let extract_bloc name head =
  let before_bloc = Str.regexp ("^.*<[ \t]*"^ name ^"[^>]*>") in
  let after_bloc = Str.regexp ("</[ \t]*"^ name ^"[^>]*>.*$") in
  Str.replace_first before_bloc "" (Str.replace_first after_bloc "" head)

let extract_meta property head =
  let meta_bloc = Str.regexp ("<[ \t]*meta[^<]*"^ property ^"[^>]*>") in
  let end_meta = Str.regexp ">" in
  try
    let start_property = Str.search_forward meta_bloc head 0 in
    let end_property = Str.search_forward end_meta head start_property in
    let length = end_property - start_property + 1 in
    let meta = String.sub head start_property length in
    let before_content = Str.regexp "^.*content ?= ?[\"']" in
    let after_content = Str.regexp "[\"'].*$" in
    Str.replace_first after_content "" (Str.replace_first before_content "" meta)
  with Not_found -> ""                  (* given meta property not found *)

let extract_meta_property name head =
  extract_meta ("property[ \t]*=[ \t]*[\"'][a-zA-Z0-9]*:?"^ name ^"[\"']") head

let extract_meta_name name head =
  extract_meta ("name[ \t]*=[ \t]*[\"']"^ name ^"[\"']") head

let clean_html dirty_html =
  let carriage_return = Str.regexp "\n" in
  let multi_space = Str.regexp "[ \t]+" in
  Str.global_replace multi_space " "
    (Str.global_replace carriage_return "" dirty_html)

let min_size = 10

let choose_best page_value property_value =
  if (String.length property_value < min_size) then page_value else property_value

let meta_of_html dirty_html =
  let html = clean_html dirty_html in
  let head = extract_bloc "head" html in
  let page_title = extract_bloc "title" head in
  let property_title = extract_meta_property "title" head in
  let page_description = extract_meta_name "description" head in
  let property_description = extract_meta_property "description" head in

  let title = choose_best page_title property_title in
  let description = choose_best page_description property_description in

  if (String.length title >= min_size && String.length description >= min_size)
  then Some { title = title; description = description }
  else None

(* let test_meta () = *)
(*   let html = String.concat "" (Utils.File.readlines "youtube.html") in *)
(*   match meta_of_html html with *)
(*   | None -> Printf.printf "Nothing found\n" *)
(*   | Some doc -> *)
(*     Printf.printf "title: '%s'\n" doc.title; *)
(*     Printf.printf "description: '%s'\n" doc.description *)

(* let _ = test_meta () *)
