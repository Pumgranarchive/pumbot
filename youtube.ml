open Utils

let is_youtube_uri uri =
  let str_uri = Ptype.string_of_uri uri in
  try ignore (Youtube_http.get_video_id_from_url str_uri); true
  with _ -> false

let video_of_song wiki_title (_, title, _) =
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
  lwt t_uris, t_subjects, t_links =
      Lwt_list.fold_left data_of_topic empty topics
  in
  lwt rt_uris, rt_subjects, rt_links =
      Lwt_list.fold_left data_of_topic empty r_topics
  in
  lwt tag_ids = Tag.Of_Content.makes (t_subjects@rt_subjects) in
  lwt () = Tag.Of_Content.assign tag_ids uri in
  lwt _ = Link.insert (t_links@rt_links) in
  Lwt.return (t_uris@rt_uris)

lwt talk_about = Tag.Of_Link.make "Talk about"
lwt mentioned_by = Tag.Of_Link.make "Mentioned by"

let wiki uri =
  let data_of_topic (p_uris, p_subjects, p_links) topic =
    lwt _,wiki_title,_,_,_,wiki_urls = Freebase_http.get_topics topic in
    let wuris = List.map Ptype.uri_of_string wiki_urls in
    let links = Link.build_inter_link [talk_about] [mentioned_by] [uri] wuris in
    Lwt.return (wuris@p_uris,
                wiki_title::p_subjects,
                links@p_links)
  in
  wrapper data_of_topic uri


lwt made_by = Tag.Of_Link.make "Made by"
lwt made = Tag.Of_Link.make "Made"
lwt same_band = Tag.Of_Link.make "Same Band"

let discography uri =
  let data_of_topic (p_uris, p_subjects, p_links) topic =
    lwt _,wiki_title,_,_,_,wiki_str_urls = Freebase_http.get_topics topic in
    let wiki_uris = List.map Ptype.uri_of_string wiki_str_urls in
    lwt song = Dbpedia_http.get_discography wiki_title in
    lwt video_uris = Lwt_list.map_p (video_of_song wiki_title) song in
    let v_uris = uri::video_uris in
    let wiki_links = Link.build_inter_link [made_by] [made] v_uris wiki_uris in
    let video_links = Link.build_each_on_all [same_band] v_uris in
    Lwt.return (video_uris@wiki_uris@p_uris,
                wiki_title::p_subjects,
                video_links@wiki_links@p_links)
  in
  wrapper data_of_topic uri

let switch uri =
  print_endline "Youtube";
  lwt to_add = discography uri in
  (* lwt to_add = wiki uri in *)
  Lwt.return to_add
