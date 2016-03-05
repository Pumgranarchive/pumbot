open Utils

(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

let () = Opencalais_http.set_token Token.opencalais

(******************************************************************************
****************************** Extract Uris ***********************************
*******************************************************************************)

let is_something_else uri = true

let wiki_action = Str.regexp "^https?://..\\.wikipedia\\.org/w/index\\.php"
let img = Str.regexp ".*\\.\\(jpg\\|jfif\\|exif\\|tiff\\|png\\|ppm\\|pgm\\|pbm\\|pnm\\|webp\\|hdr\\|heif\\|bpg\\|cgm\\|svg\\|bmp\\|gif\\|ico\\)\\(\\?.*\\)?$"
let not_human_readable = Str.regexp ".*\\.\\(js\\|css\\|json\\)\\(\\?.*\\)?$"
let not_managed = Str.regexp ".*\\.\\(xml\\|pdf\\)\\(\\?.*\\)?$"
let wiki_category = Str.regexp ".*/wiki/[a-z]*:.*"
let mailto = Str.regexp ".*mailto:.*"
let should_be_removed uri =
  Str.string_partial_match wiki_action uri 0 ||
  Str.string_match wiki_category uri 0 ||
  Str.string_match img uri 0 ||
  Str.string_match not_human_readable uri 0 ||
  Str.string_match not_managed uri 0 ||
  Str.string_match mailto uri 0

let ink = Str.regexp "#.*"
let cand = Str.regexp "&.*"
let before_quote = Str.regexp "^.*[\"']"
let after_quote = Str.regexp "[\"'].*$"
let clean_uri href =
  let uri = Str.replace_first after_quote ""
    (Str.replace_first before_quote "" href)
  in
  Str.global_replace cand "" (Str.global_replace ink "" uri)

let protocol = Str.regexp "^https?://.*$"
let double_slash_start = Str.regexp "^//.*$"
let slash_start = Str.regexp "^/.*$"
let real_path domain directory url =
  (* Printf.printf "url: '%s'\n" url; *)
  let str_match regex = Str.string_match regex url 0 in
  if str_match protocol then url else                           (* abosulte  *)
    if str_match double_slash_start then "http:" ^ url else     (* without protocol *)
      if str_match slash_start then domain ^ url else           (* root *)
        directory ^ url                                         (* in dir *)

let compare s1 s2 = String.compare s1 (String.lowercase s2) == 0

let uri_expr = Str.regexp "\\(src\\|href\\|action\\)[ \t]*=[ \t]*[\"'][ \t]*[a-zA-Z0-9\\+&\\@#/%=~_|!:,\\.;\\-\\?]+[ \t]*[\"']"
let end_uri = Str.regexp "[^=][ \t]*[\"']"
let rec extract_uri domain directory body uris pos =
  try
    let start_p = Str.search_forward uri_expr body pos in
    let end_p = Str.search_forward end_uri body (start_p + 1) in
    let length = end_p - start_p + 1 in
    let uri = clean_uri (String.sub body start_p length) in
    let real_uri = real_path domain directory uri in
    let lower_uri = String.lowercase real_uri in
    if (should_be_removed lower_uri || List.exists (compare lower_uri) uris)
    then extract_uri domain directory body uris (end_p + 1)
    else extract_uri domain directory body (real_uri::uris) (end_p + 1)
  with Not_found -> uris

let file = Str.regexp "[^/]*$"
let current_directory url =
  Str.replace_first file "" url

let end_of_protocol = Str.regexp "://"
let slash = Str.regexp "/"
let domain_of_uri url =
  try
    let start_domain = Str.search_forward end_of_protocol url 0 + 3 in
    let end_domain = Str.search_forward slash url start_domain in
    let length = end_domain - start_domain in
    "http://" ^ String.sub url start_domain length
  with Not_found -> "http://"

(******************************************************************************
******************************* Roles Utils ***********************************
*******************************************************************************)

let open_bloc_regex = Str.regexp "<"
let end_bloc_regex = Str.regexp ">"
let close_bloc_regex = Str.regexp "^<[ \t]*/.*$"
let not_alphanum = Str.regexp "[^a-zA-Z0-9]"

let rec search_end_bloc html bloc_name pos count =
  if (count == 0)
  then Str.search_forward end_bloc_regex html pos + 1
  else
    let bloc_regex = Str.regexp ("<[ \t]*/?[ \t]*" ^ bloc_name) in
    let start_role = Str.search_forward bloc_regex html pos in
    (* Printf.printf "bloc for %s<br />" bloc_name; *)
    let length = String.length bloc_name + 2 in
    let start_balise = String.sub html start_role length in
    (* Printf.printf "balise <!-- %s --><br />" start_balise; *)
    let new_count =
      if Str.string_match close_bloc_regex start_balise 0
      then count - 1
      else count + 1
    in
    search_end_bloc html bloc_name (start_role + length) new_count

let get_role_position role_name html =
  try
    let role_regex = Str.regexp ("role[ \t]*=[ \t]*[\"']\\.?" ^ role_name ^ "[\"']") in
    let start_role = Str.search_forward role_regex html 0 in
    (* Printf.printf "Found %s<br />" role_name; *)
    let start_bloc = Str.search_backward open_bloc_regex html start_role in
    (* Printf.printf "Found bloc %s<br />" role_name; *)
    let end_name = Str.search_forward not_alphanum html (start_bloc + 1) in
    let bloc_name = String.sub html (start_bloc + 1) (end_name - start_bloc - 1) in
    (* Printf.printf "Found bloc name '%s' '%s'<br />" (String.sub html start_bloc 1) bloc_name; *)
    let end_bloc = search_end_bloc html bloc_name end_name 1 in
    (* Printf.printf "End bloc for %s<br />" bloc_name; *)
    Some((start_bloc, end_bloc))
  with Not_found ->
    (* Printf.printf "Not Found for %s<br /><br />" role_name; *)
    None

let get_role role_name html =
  match get_role_position role_name html with
  | None                         -> None
  | Some((start_bloc, end_bloc)) ->
    Some(String.sub html start_bloc (end_bloc - start_bloc))

let remove_role role_name html =
  match get_role_position role_name html with
  | None                         -> None
  | Some((start_bloc, end_bloc)) ->
    let length = String.length html - end_bloc in
    let before = String.sub html 0 start_bloc in
    let after = String.sub html end_bloc length in
    Some(before ^ after)

let rec remove_all_role role_name html =
  match remove_role role_name html with
  | Some(new_html) -> remove_all_role role_name new_html
  | None           -> html

let get_expected_role html =
  let heading = get_role "heading" html in
  let main = get_role "main" html in
  Option.join (^) heading main

let unexpected_roles = ["banner"; "navigation"; "search"; "contentinfo"]
let remove_unexpected_role html =
  List.fold_right remove_all_role unexpected_roles html

(******************************************************************************
******************************** Bloc Utils ***********************************
*******************************************************************************)

let extract_bloc name head =
  let before_bloc = Str.regexp ("^.*<[ \t]*"^ name ^"[^>]*>") in
  let after_bloc = Str.regexp ("</[ \t]*"^ name ^"[^>]*>.*$") in
  Str.replace_first before_bloc "" (Str.replace_first after_bloc "" head)

let remove_bloc start_regex end_regex html =
  try
    let start_regex = Str.regexp start_regex in
    let end_regex = Str.regexp end_regex in
    let start_bloc = Str.search_forward start_regex html 0 in
    let start_end_bloc = Str.search_forward end_regex html start_bloc in
    let end_bloc = Str.search_forward end_bloc_regex html start_end_bloc + 1 in
    let length = String.length html - end_bloc in
    let before = String.sub html 0 start_bloc in
    let after = String.sub html end_bloc length in
    Some(before ^ after)
  with Not_found -> None

let rec remove_all_bloc start_regex end_regex html =
  match remove_bloc start_regex end_regex html with
  | Some(new_html) -> remove_all_bloc start_regex end_regex new_html
  | None           -> html

let remove_comment html = remove_all_bloc "<!\\-\\-" "\\-\\->" html
let remove_cdata html = remove_all_bloc "<!\\[CDATA\\[" "\\]\\]>" html
let remove_head html = remove_all_bloc "<[ \t]*head" "<[ \t]*/[ \t]*head" html
let remove_script html = remove_all_bloc "<[ \t]*script" "<[ \t]*/[ \t]*script" html
let remove_header html = remove_all_bloc "<[ \t]*header" "<[ \t]*/[ \t]*header" html
let remove_nav html = remove_all_bloc "<[ \t]*nav" "<[ \t]*/[ \t]*nav" html
let remove_footer html = remove_all_bloc "<[ \t]*footer" "<[ \t]*/[ \t]*footer" html

(* Does not remove head because xtractor may used it *)
(* Does not remove script because of bug introducting... *)
let unexpected_balises = [remove_comment; remove_cdata;
                          remove_header; remove_nav; remove_footer]
let clean_unexpected_balise html =
  List.fold_left (fun html f -> f html) html unexpected_balises

let clean_html html =
  remove_unexpected_role (clean_unexpected_balise html)

(* let test_html () = *)
(*   let html = String.concat "" (File.readlines "bbc.html") in *)
(*   print_endline (clean_html html) *)
(*   (\* match get_expected_role html with *\) *)
(*   (\* | None       -> print_endline "No bloc found" *\) *)
(*   (\* | Some(bloc) -> print_endline bloc *\) *)

(* let _ = test_html () *)

(******************************************************************************
***************************** Contained Uris **********************************
*******************************************************************************)

let contained_uris_of_html base_uri html =
  let base_str_uri = Ptype.string_of_uri base_uri in
  let directory = current_directory base_str_uri in
  let domain = domain_of_uri base_str_uri in
  let str_uris = extract_uri domain directory html [] 0 in
  let uris = List.rev (List.map Ptype.uri_of_string str_uris) in
  List.limit 40 uris

(* let test_uris () = *)
(*   let html = String.concat "" (Utils.File.readlines "bbc.html") in *)
(*   let home_bbc = Ptype.uri_of_string "http://www.bbc.com/" in *)
(*   let bbc = Ptype.uri_of_string "http://www.bbc.com/earth/story/20151030-photography-battle-new-zealand-v-australia" in *)
(*   let youtube = Ptype.uri_of_string "https://www.youtube.com/watch?v=BX2MtlrhSZk" in *)
(*   let wikipedia = Ptype.uri_of_string "https://en.wikipedia.org/wiki/Eternity_II_puzzle" in *)
(*   let uris = contained_uris_of_html bbc html in *)
(*   List.iter (fun uri -> print_endline (Ptype.string_of_uri uri)) uris *)

(* let _ = test_uris () *)


(******************************************************************************
***************************** Extract html meta *******************************
*******************************************************************************)

type meta = { title: string; description: string }

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

let compact_html dirty_html =
  let carriage_return = Str.regexp "\n" in
  let multi_space = Str.regexp "[ \t]+" in
  Str.global_replace multi_space " "
    (Str.global_replace carriage_return "" dirty_html)

let min_size = 10

let choose_best page_value property_value =
  if (String.length property_value < min_size) then page_value else property_value

let meta_of_html dirty_html =
  let html = compact_html dirty_html in
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
