open Utils

module Yojson = Yojson.Basic

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

lwt wikipedia_tag = Tag.Of_Content.make "Wikipedia"
lwt talk_about = Tag.Of_Link.make "Talk about"
lwt mentioned_by = Tag.Of_Link.make "Mentioned by"

(******************************************************************************
********************************* Getters *************************************
*******************************************************************************)

let is_wikipedia_uri uri =
  Dbpedia_http.is_wikipedia_uri (Ptype.string_of_uri uri)

let get uri =
  let aux () =
    print_endline "Dbpedia";
    let open Dbpedia_record.Basic in
    let str_uri = Ptype.string_of_uri uri in
    lwt data = Lwt_list.hd (Dbpedia_http.get_basic_informations_by_uri str_uri) in
    let format_subject subject =
      let subject = Str.replace_first (Str.regexp "^.*:") "" subject in
      Str.global_replace (Str.regexp "[_\\.]") " " subject
    in
    let subjects = List.map format_subject data.subject in
    lwt tag_ids = Tag.Of_Content.makes subjects in
    let tags = wikipedia_tag::tag_ids in
    lwt title, body = Readability.get_data uri in
    let wuris = Readability.get_contained_uris body in
    lwt yuris = Youtube.search title in
    let uris = yuris@wuris in
    let links = Link.build_inter_link [talk_about] [mentioned_by] [uri] uris in
    Lwt.return (tags, links, uris)
  in
  try_lwt aux ()
  with _ -> Readability.get uri
