open Utils

let from_video video =
  lwt tags_uri = Pumgrana.insert_tags Ptype.Link ["Made by";"Made";"Same Band"]
  in
  let made_by = List.nth tags_uri 0 in
  let made = List.nth tags_uri 1 in
  let same_band = List.nth tags_uri 2 in
  let (_,_,str_url,_,categories) = video in
  let url = Ptype.uri_of_string str_url in
  let (topics, _) = categories in
  let get_links topic =
    lwt (_,wiki_title,_,_,_,wiki_str_urls) = Freebase_http.get_topics topic in
    let wiki_urls = List.map Ptype.uri_of_string wiki_str_urls in
    lwt song = Dbpedia_http.get_discography wiki_title in
    let wiki_links video_urls wiki_urls =
      let single video_url blist wiki_url =
        (video_url, wiki_url, [made_by])::
          ((wiki_url, video_url, [made])::blist)
      in
      let on_videos blist v_url =
        List.fold_left (single v_url) blist wiki_urls
      in
      List.fold_left on_videos [] video_urls
    in
    let video_url (_, title, _) =
      lwt videos = Youtube_http.search_video (wiki_title ^.^ title) 1 in
      let (_, _, video_str_url, _, _) = List.hd videos in
      Lwt.return (Ptype.uri_of_string video_str_url)
    in
    let video_links videos_url current_url =
      let single blist url =
        if Ptype.compare_uri url current_url = 0
        then blist
        else (url, current_url, [same_band])::blist
      in
      List.fold_left single [] videos_url
    in
    lwt video_urls = Lwt_list.map_p video_url song in
    let urls = url::video_urls in
    let wiki_links = wiki_links urls wiki_urls in
    let video_links = List.concat (List.map (video_links urls) urls) in
    Lwt.return (video_links@wiki_links)
  in
  Lwt_list.concat (Lwt_list.map_s get_links topics)
