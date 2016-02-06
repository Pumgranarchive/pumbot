open Utils

module Yojson = Yojson.Basic

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let body_of uri =
  lwt body = Boilerpipe.boilerpipe Boilerpipe.Default uri in
  Lwt.return (String.concat "" body)

let data_of uri =
  lwt body = Boilerpipe.boilerpipe Boilerpipe.Default uri in
  let title = List.hd body in
  let content = String.concat "" (List.tl body) in
  print_endline title;
  print_endline content;
  let summary = Str.limit content 100 in
  Lwt.return (title, summary, content)

let talk_about = "Talk about"
let mentioned_by = "Mentioned by"

(******************************************************************************
******************************** Functions ************************************
*******************************************************************************)

let get uri =
  print_endline "Boilerpipe";

  lwt title, summary, body = data_of uri in
  lwt json = Opencalais_http.request body in
  let subjects = Opencalais_http.to_social_tags json in
  print_endline "\n## Subject";
  List.iter print_endline subjects;
  print_endline "## End of subject\n";
  let tags = Tag.makes subjects in
  let content = Content.make uri title summary tags in

  let buris = ExtractTools.contained_uris_of_html uri body in
  let rlinks = Link.build_inter_link talk_about mentioned_by [uri] buris in

  lwt yuris = Youtube.search title in
  let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
  let _ = Link.insert ylinks in (* Always insert youtube links  *)

  let uris = yuris@buris in

  Lwt.return (content, rlinks, uris)
