open Utils

type document = Xtractor.document

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let body_of uri =
  let open Xtractor in
  lwt doc = Xtractor.xtractor uri in
  Lwt.return doc.body

let data_of = Xtractor.xtractor

let talk_about = "Talk about"
let mentioned_by = "Mentioned by"

(******************************************************************************
******************************** Functions ************************************
*******************************************************************************)

let get uri =
  print_endline "Xtractor";

  lwt doc = data_of uri in
  lwt json = Opencalais_http.request doc.Xtractor.body in
  let subjects = Opencalais_http.to_social_tags json in
  print_endline "\n## Subject";
  List.iter print_endline subjects;
  print_endline "## End of subject\n";
  let tags = Tag.makes subjects in
  let content = Content.make uri doc.Xtractor.title doc.Xtractor.summary tags in

  let buris = ExtractTools.contained_uris_of doc.Xtractor.content in
  let rlinks = Link.build_inter_link talk_about mentioned_by [uri] buris in

  lwt yuris = Youtube.search doc.Xtractor.title in
  let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
  let _ = Link.insert ylinks in (* Always insert youtube links  *)

  let uris = yuris@buris in

  Lwt.return (content, rlinks, uris)
