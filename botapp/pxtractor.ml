open Utils

type document = { title: string; body: string; summary: string; content: string }

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

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
  with Not_found -> ""

let clean_html dirty_html =
  let carriage_return = Str.regexp "\n" in
  let multi_space = Str.regexp "[ \t]+" in
  Str.global_replace multi_space " "
    (Str.global_replace carriage_return "" dirty_html)

let get_best page_value property_value =
  if (String.length property_value < 5) then page_value else property_value

let document_of_html dirty_html =
  let html = clean_html dirty_html in
  let head = extract_bloc "head" html in
  let page_title = extract_bloc "title" head in
  let property_title = extract_meta ":?title" head in
  let page_description = extract_meta "name ?= ?[\"']description" head in
  let property_description = extract_meta ":?description" head in

  let title = get_best page_title property_title in
  let description = get_best page_description property_description in

  if (String.length title < 5 || String.length description < 5) then None
  else Some {title= title; body= description; summary= description; content= html}

(* let test () = *)
(*   let html = String.concat "" (Utils.File.readlines "bbc.html") in *)
(*   match document_of_html html with *)
(*   | None -> Printf.printf "Nothing found\n" *)
(*   | Some doc -> *)
(*     Printf.printf "title: '%s'\n" doc.title; *)
(*     Printf.printf "description: '%s'\n" doc.summary *)

(* let _ = test () *)

let to_document html lwt_doc =
  lwt doc = lwt_doc in
  Lwt.return (Some {
    title = doc.Xtractor.title;
    body = doc.Xtractor.body;
    summary = doc.Xtractor.summary;
    content = html })

let data_of uri =
  lwt (response, body) = Pghttp.get uri in
  let status = Cohttp_lwt_unix.Response.status response in
  let code = Cohttp.Code.code_of_status status in
  if (code >= 200 && code < 300) then           (* Good response *)
    lwt html = Cohttp_lwt_body.to_string body in
    match document_of_html html with
    | Some doc -> Lwt.return (Some doc)
    | None     -> to_document html (Xtractor.xtractor uri html)
  else                                          (* An error occured *)
    Lwt.return None

let body_of uri =
  lwt opt_doc = data_of uri in
  match opt_doc with
  | Some doc  -> Lwt.return (Some doc.body)
  | None      -> Lwt.return None

let talk_about = "Talk about"
let mentioned_by = "Mentioned by"

(******************************************************************************
******************************** Functions ************************************
*******************************************************************************)

let linker uri doc =

  lwt json = Opencalais_http.request doc.body in
  let subjects = Opencalais_http.to_social_tags json in
  (* print_endline "\n## Subject"; *)
  (* List.iter print_endline subjects; *)
  (* print_endline "## End of subject\n"; *)

  let tags = Tag.makes subjects in
  let content = Content.make uri doc.title doc.summary tags in

  let buris = ExtractTools.contained_uris_of doc.content in
  let rlinks = Link.build_inter_link talk_about mentioned_by [uri] buris in

  lwt yuris = Youtube.search doc.title in
  let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
  let _ = Link.insert ylinks in (* Always insert youtube links  *)

  let uris = yuris@buris in

  Lwt.return (Some (content, rlinks, uris))


let get uri =
  print_endline "Xtractor";
  lwt opt_doc = data_of uri in
  match opt_doc with
  | None     -> Lwt.return None
  | Some doc -> linker uri doc
