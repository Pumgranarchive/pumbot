open Utils

module Yojson = Yojson.Basic

(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

let () = Readability_http.set_token Token.readability

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

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

let get uri =
  print_endline "Readability";

  lwt title, summary, body = get_data uri in
  lwt json = Opencalais_http.request body in
  let subjects = Opencalais_http.to_social_tags json in
  let tags = Tag.makes subjects in
  let content = Content.make uri title summary tags in

  let buris = ExtractTools.contained_uris_of_html uri body in
  let rlinks = Link.build_inter_link talk_about mentioned_by [uri] buris in

  lwt yuris = Youtube.search title in
  let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
  let _ = Link.insert ylinks in (* Always insert youtube links  *)

  let uris = yuris@buris in

  Lwt.return (content, rlinks, uris)
