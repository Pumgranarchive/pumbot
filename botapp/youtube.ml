open Utils

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let uri_of_video (_, _, video_str_url, _, _) = Ptype.uri_of_string video_str_url

let is_youtube_uri uri =
  let str_uri = Ptype.string_of_uri uri in
  try ignore (Youtube_http.get_video_id_from_url str_uri); true
  with _ -> false

let video_uri_of_song wiki_title (_, title, _) =
  lwt videos = Youtube_http.search_video ~query:(wiki_title ^.^ title) 1 in
  let _, _, video_str_url, _, _ = List.hd videos in
  Lwt.return (Ptype.uri_of_string video_str_url)

let search text =
  lwt videos = Youtube_http.search_video ~query:(text ^.^ "film") 8 in
  Printf.printf "youtube %d (%s film)\n" (List.length videos) text;
  let videos_uri = List.map uri_of_video videos in
  Lwt.return videos_uri

let video_of_uri uri =
  let url = Ptype.string_of_uri uri in
  let id = Youtube_http.get_video_id_from_url url in
  lwt videos = Youtube_http.get_videos_from_ids [id] in
  Lwt.return (List.hd videos)

let youtube_tag = Tag.make "Youtube"

(*
**
** get freebase informations from an URI.
** apply "data_of_topic" on "topics" and "r_topics"
** "data_of_topic" have to return "uris, subjects, links"
** "subjects" and "links" will be assigned and inserted to the current content
** "uris" will be returned for (possible) future usage
**
*)
let wrapper data_of_topic uri =
  lwt id, title, str_url, summary, categories = video_of_uri uri in
  let topics, r_topics = categories in
  let empty = ([], [], []) in
  lwt data = Lwt_list.dep_fold_left data_of_topic empty [topics;r_topics] in
  let uris, data_subjects, links = data in
  let data_tags = Tag.makes data_subjects in
  let tags = youtube_tag::data_tags in
  let content = Content.make uri title summary tags in
  Lwt.return (content, links, uris)

let append_data previous_data new_data =
  let p_uris, p_subjects, p_links = previous_data in
  let n_uris, n_subjects, n_links = new_data in
  n_uris@p_uris, n_subjects@p_subjects, n_links@p_links


(******************************************************************************
******************************* Search By Topic  *****************************
*******************************************************************************)
let search_by_topic topic =
  Youtube_http.search_video ~topic_id:topic 10

(******************************************************************************
********************************** Wiki ***************************************
*******************************************************************************)

let talk_about = "Talk about"
let mentioned_by = "Mentioned by"

let build_wiki_data uris (t1, t2) p_data wiki_data =
  let _,wiki_title,_,_,_,wiki_urls = wiki_data in
  let wuris = List.map Ptype.uri_of_string wiki_urls in
  let links = Link.build_inter_link t1 t2 uris wuris in
  append_data p_data (wuris, [wiki_title], links)

let wiki uri =
  let data_of_topic p_data topic =
    lwt wiki_data = Freebase_http.get_topics topic in
    let tags = talk_about, mentioned_by in
    Lwt.return (build_wiki_data [uri] tags p_data wiki_data)
  in
  wrapper data_of_topic uri

(******************************************************************************
******************************* Discography ***********************************
*******************************************************************************)

let made_by = "Made by"
let made = "Made"
let same_band = "Same Band"

let discography uri =
  let data_of_topic p_data topic =
    lwt freebase_data = Freebase_http.get_topics topic in
    let _,freebase_title,_,_,_,freebase_str_urls = freebase_data in
    print_endline "Dbpedia get discography";
    lwt songs = Dbpedia_http.get_discography freebase_title in
    print_endline "Dbpedia done discography";
    lwt video_uris = Lwt_list.map_p (video_uri_of_song freebase_title) songs in
    let v_uris = uri::video_uris in
    let video_links = Link.build_each_on_all same_band v_uris in
    let tags = made_by, made in
    let new_data = build_wiki_data v_uris tags p_data freebase_data in
    Lwt.return (append_data new_data (video_uris, [], video_links))
  in
  wrapper data_of_topic uri


(******************************************************************************
********************************** Switch *************************************
*******************************************************************************)

let switch uri =
  print_endline "Youtube";
  (* lwt to_add = discography uri in *)
  lwt to_add = wiki uri in
  Lwt.return to_add
