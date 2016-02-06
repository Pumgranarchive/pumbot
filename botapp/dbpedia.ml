open Utils

module Yojson = Yojson.Basic

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let wikipedia_tag = Tag.make "Wikipedia"

let talk_about = "Talk about"
let mentioned_by = "Mentioned by"

(******************************************************************************
********************************* Getters *************************************
*******************************************************************************)

let is_wikipedia_uri uri =
  Dbpedia_http.is_wikipedia_uri (Ptype.string_of_uri uri)

let linker uri data doc =
    let open Dbpedia_record.Basic in
    let open Pxtractor in

    let format_subject subject =
      let subject = Str.replace_first (Str.regexp "^.*:") "" subject in
      Str.global_replace (Str.regexp "[_\\.]") " " subject
    in

    let data_subjects = List.map format_subject data.subject in
    let data_tags = Tag.makes data_subjects in
    let tags = wikipedia_tag::data_tags in
    let content = Content.make uri doc.title doc.summary tags in

    let wuris = ExtractTools.contained_uris_of_html uri doc.content in
    let wlinks = Link.build_inter_link talk_about mentioned_by [uri] wuris in

    lwt yuris = Youtube.search doc.title in
    let ylinks = Link.build_inter_link talk_about mentioned_by [uri] yuris in
    let _ = Link.insert ylinks in (* Always insert youtube links  *)

    let uris = yuris@wuris in
    Lwt.return (Some (content, wlinks, uris))

let get uri =
  let aux () =
    print_endline "Dbpedia";

    let str_uri = Ptype.string_of_uri uri in
    print_endline str_uri;
    print_endline "Dbpedia get basic informations";
    lwt data_list = Dbpedia_http.get_basic_informations_by_uri str_uri in
    if List.length data_list == 0 then Lwt.return None else
      lwt opt_doc = Pxtractor.data_of uri in
      match opt_doc with
      | None     -> Lwt.return None
      | Some doc -> linker uri (List.hd data_list) doc

  in
  try_lwt aux ()
  with e ->
    begin
      print_endline ("\n[Error][Dbpedia] :: " ^ (Printexc.to_string e));
      print_endline "Try ...\n";
      Pxtractor.get uri
    end
