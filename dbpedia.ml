open Utils

let is_wikipedia_uri uri =
  Dbpedia_http.is_wikipedia_uri (Ptype.string_of_uri uri)

lwt wikipedia_tag = Tag.Of_Content.make "Wikipedia"
let () = print_endline (Ptype.string_of_uri wikipedia_tag)

let get uri =
  print_endline "Dbpedia";
  let open Dbpedia_record.Basic in
  let str_uri = Ptype.string_of_uri uri in
  lwt data = Lwt_list.hd (Dbpedia_http.get_basic_informations_by_uri str_uri) in
  let format_subject subject =
    let subject = Str.replace_first (Str.regexp "^.*:") "" subject in
    Str.global_replace (Str.regexp "[_\.]") " " subject
  in
  let subjects = List.map format_subject data.subject in
  lwt tag_ids = Tag.Of_Content.makes subjects in
  lwt () = Tag.Of_Content.assign (wikipedia_tag::tag_ids) uri in
  Lwt.return []
