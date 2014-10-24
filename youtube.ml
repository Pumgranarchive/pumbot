open Utils

let is_youtube_uri uri =
  let str_uri = Ptype.string_of_uri uri in
  try ignore (Youtube_http.get_video_id_from_url str_uri); true
  with _ -> false

let video_of_song wiki_title (_, title, _) =
  lwt videos = Youtube_http.search_video (wiki_title ^.^ title) 1 in
  let _, _, video_str_url, _, _ = List.hd videos in
  Lwt.return (Ptype.uri_of_string video_str_url)


lwt talk_about = Tag.Of_Link.make "Talk about"
lwt mentioned_by = Tag.Of_Link.make "Mentioned by"

let wiki uri =
  let url = Ptype.string_of_uri uri in
  let id = Youtube_http.get_video_id_from_url url in
  lwt videos = Youtube_http.get_videos_from_ids [id] in
  let (id, title, str_url, summary, categories) = List.hd videos in
  let (topics, r_topics) = categories in
  let links_of_topic (p_wikis, p_names, p_links) topic =
    lwt (_,name,_,_,_,wiki_urls) = Freebase_http.get_topics topic in
    let wiki_uris = List.map Ptype.uri_of_string wiki_urls in
    let links =
      Link.build_inter_link [talk_about] [mentioned_by] [uri] wiki_uris
    in
    Lwt.return (wiki_urls::p_wikis, name::p_names, links::p_links)
  in
  let assign_tag tag_ids content =
    let uri = Ptype.uri_of_string str_url in
    Pumgrana.update_content_tags uri tag_ids
  in
  let empty = ([], [], []) in
  lwt wikis, tags, links = Lwt_list.fold_left links_of_topic empty topics in
  lwt r_wikis, r_tags, r_links = Lwt_list.fold_left links_of_topic empty r_topics
  in
  let links = List.concat (links@r_links) in
  let subjects = tags@r_tags in
  lwt tag_ids = Tag.Of_Content.makes subjects in
  let contents = List.concat (wikis@r_wikis@[[str_url]]) in
  lwt () = Lwt_list.iter_p (assign_tag tag_ids) contents in
  let () = Link.print links in
  lwt _ = Pumgrana.insert_links links in
  Lwt.return []


lwt made_by = Tag.Of_Link.make "Made by"
lwt made = Tag.Of_Link.make "Made"
lwt same_band = Tag.Of_Link.make "Same Band"

let discography uri =
  let url = Ptype.string_of_uri uri in
  let id = Youtube_http.get_video_id_from_url url in
  lwt videos = Youtube_http.get_videos_from_ids [id] in
  let id, title, str_url, summary, categories = List.hd videos in
  let topics, rel_topics = categories in
  let links_of_topic topic =
    lwt _,wiki_title,_,_,_,wiki_str_urls = Freebase_http.get_topics topic in
    let wiki_urls = List.map Ptype.uri_of_string wiki_str_urls in
    lwt song = Dbpedia_http.get_discography wiki_title in
    lwt video_urls = Lwt_list.map_p (video_of_song wiki_title) song in
    let uris = uri::video_urls in
    let wiki_links = Link.build_inter_link [made_by] [made] uris wiki_urls in
    let video_links = Link.build_each_on_all [same_band] uris in
    Lwt.return (video_links@wiki_links)
  in
  lwt t_links = Lwt_list.concat (Lwt_list.map_s links_of_topic topics) in
  lwt rt_links = Lwt_list.concat (Lwt_list.map_s links_of_topic rel_topics) in
  let links = t_links@rt_links in
  let () = Link.print links in
  lwt _ = Pumgrana.insert_links links in
  Lwt.return []

let switch uri =
  print_endline "Youtube";
  lwt to_add = discography uri in
  (* lwt to_add = wiki uri in *)
  Lwt.return to_add
