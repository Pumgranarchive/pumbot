open Utils

type document = { title: string; body: string; summary: string; content: string }

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let document_of_meta html meta =
  Printf.printf "Meta\n";
  Lwt.return (Some {
    title = meta.ExtractTools.title;
    body = meta.ExtractTools.description;
    summary = meta.ExtractTools.description;
    content = html })

let document_of_xtractor html doc =
  print_endline "Done";
  Lwt.return (Some {
    title = doc.Xtractor.title;
    body = doc.Xtractor.body;
    summary = doc.Xtractor.summary;
    content = html })

let data_of uri =
  print_endline "Querying ...";
  lwt (response, body) = Pghttp.get uri in
  print_endline "Done querying ...";
  let status = Cohttp_lwt_unix.Response.status response in
  let code = Cohttp.Code.code_of_status status in
  Printf.printf "Code %d\n" code;
  if (code >= 200 && code < 300) then           (* Good response *)
    lwt html = Cohttp_lwt_body.to_string body in
    let clean_html = ExtractTools.clean_html html in
    match ExtractTools.meta_of_html clean_html with
    | Some meta -> document_of_meta clean_html meta
    | None      ->
      print_endline "No meta, querying xtractor...";
      lwt doc = Xtractor.xtractor uri clean_html in
      document_of_xtractor clean_html doc
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

  let buris = ExtractTools.contained_uris_of_html uri doc.content in
  let rlinks = Link.build_inter_link talk_about mentioned_by [uri] buris in

  lwt yuris = Youtube.search doc.title in
  let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
  let _ = Link.insert ylinks in (* Always insert youtube links  *)

  let uris = yuris@buris in

  Lwt.return (Some (content, rlinks, uris))


let get uri =
  let str_uri = Ptype.string_of_uri uri in
  print_endline ("Xtractor "^ str_uri);
  lwt opt_doc = data_of uri in
  match opt_doc with
  | None     -> Lwt.return None
  | Some doc -> linker uri doc
