open Utils

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let is_youtube_uri uri =
  let str_uri = Ptype.string_of_uri uri in
  try ignore (Youtube_http.get_video_id_from_url str_uri); true
  with _ -> false

let video_uri_of_song wiki_title (_, title, _) =
  lwt videos = Youtube_http.search_video (wiki_title ^.^ title) 1 in
  let _, _, video_str_url, _, _ = List.hd videos in
  Lwt.return (Ptype.uri_of_string video_str_url)

let video_of_uri uri =
  let url = Ptype.string_of_uri uri in
  let id = Youtube_http.get_video_id_from_url url in
  lwt videos = Youtube_http.get_videos_from_ids [id] in
  Lwt.return (List.hd videos)

let wrapper data_of_topic uri =
  lwt id, title, str_url, summary, categories = video_of_uri uri in
  let topics, r_topics = categories in
  let empty = ([], [], []) in
  lwt data = Lwt_list.dep_fold_left data_of_topic empty [topics;r_topics] in
  let uris, subjects, links = data in
  lwt tag_ids = Tag.Of_Content.makes subjects in
  lwt () = Tag.Of_Content.assign tag_ids uri in
  lwt _ = Link.insert links in
  Lwt.return uris

let append_data previous_data new_data =
  let p_uris, p_subjects, p_links = previous_data in
  let n_uris, n_subjects, n_links = new_data in
  n_uris@p_uris, n_subjects@p_subjects, n_links@p_links

(******************************************************************************
********************************** Wiki ***************************************
*******************************************************************************)

lwt talk_about = Tag.Of_Link.make "Talk about"
lwt mentioned_by = Tag.Of_Link.make "Mentioned by"

let build_wiki_data uris (t1, t2) p_data wiki_data =
  let _,wiki_title,_,_,_,wiki_urls = wiki_data in
  let wuris = List.map Ptype.uri_of_string wiki_urls in
  let links = Link.build_inter_link t1 t2 uris wuris in
  append_data p_data (wuris, [wiki_title], links)

let wiki uri =
  let data_of_topic p_data topic =
    lwt wiki_data = Freebase_http.get_topics topic in
    let tags = [talk_about], [mentioned_by] in
    Lwt.return (build_wiki_data [uri] tags p_data wiki_data)
  in
  wrapper data_of_topic uri

(******************************************************************************
******************************* Discography ***********************************
*******************************************************************************)

lwt made_by = Tag.Of_Link.make "Made by"
lwt made = Tag.Of_Link.make "Made"
lwt same_band = Tag.Of_Link.make "Same Band"

let discography uri =
  let data_of_topic p_data topic =
    lwt wiki_data = Freebase_http.get_topics topic in
    let _,wiki_title,_,_,_,wiki_str_urls = wiki_data in
    lwt songs = Dbpedia_http.get_discography wiki_title in
    lwt video_uris = Lwt_list.map_p (video_uri_of_song wiki_title) songs in
    let v_uris = uri::video_uris in
    let video_links = Link.build_each_on_all [same_band] v_uris in
    let tags = [made_by], [made] in
    let new_data = build_wiki_data v_uris tags p_data wiki_data in
    Lwt.return (append_data new_data (video_uris, [], video_links))
  in
  wrapper data_of_topic uri

(******************************************************************************
********************************** Switch *************************************
*******************************************************************************)

let switch uri =
  print_endline "Youtube";
  lwt to_add = discography uri in
  (* lwt to_add = wiki uri in *)
  Lwt.return to_add
