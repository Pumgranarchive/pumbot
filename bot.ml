let lwt_hd list =
  lwt l = list in
  Lwt.return (List.hd l)

let lwt_concat lists =
  lwt ls = lists in
  Lwt.return (List.concat ls)

let (^.^) a b = a ^ " " ^ b

let discography_links_from_video video =
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
  lwt_concat (Lwt_list.map_s get_links topics)

let wiki_links_from_video video =
  lwt tags_uri = Pumgrana.insert_tags Ptype.Link ["Talk about";"Mentioned by"]
  in
  let talk_about = List.nth tags_uri 0 in
  let mentioned_by = List.nth tags_uri 1 in
  let (_,_,str_url,_,categories) = video in
  let url = Ptype.uri_of_string str_url in
  let (topics, rel_topics) = categories in
  let get_links topic =
    lwt (_,_,_,_,_,wiki_urls) = Freebase_http.get_topics topic in
    let format wiki_url =
      let wiki_url = Ptype.uri_of_string wiki_url in
      [(url, wiki_url, [talk_about]);
       (wiki_url, url, [mentioned_by])]
    in
    let links = List.concat (List.map format wiki_urls) in
    Lwt.return links
  in
  lwt wiki_links = Lwt_list.map_p get_links topics in
  lwt rel_wiki_links = Lwt_list.map_p get_links rel_topics in
  Lwt.return (List.concat (wiki_links@rel_wiki_links))

let ids = List.map Youtube_http.get_video_id_from_url
  (* ["https://www.youtube.com/watch?v=op-c3njOKz0"] *)
  ["https://www.youtube.com/watch?v=DonHa4pySsM"]
  (* ["https://www.youtube.com/watch?v=MZeh07V9vRo&list=TL7holR59tY4PTd7DruZVsGT7IzQ30N3A7"] *)

let print_link (o, t, tags) =
  Printf.printf
    "o:%s\nt:%s\ntags:%s\n\n"
    (Ptype.string_of_uri o)
    (Ptype.string_of_uri t)
    (Ptype.string_of_uri (List.hd tags))

let map_links func list =
  lwt_concat (Lwt_list.map_p func list)

let create_wiki_links ids =
  lwt videos = Youtube_http.get_videos_from_ids ids in
  lwt links = map_links wiki_links_from_video videos in
  List.iter print_link links;
  lwt _ = Pumgrana.insert_links links in
  Lwt.return ()

let create_discography_links ids =
  lwt videos = Youtube_http.get_videos_from_ids ids in
  lwt links = map_links discography_links_from_video videos in
  List.iter print_link links;
  lwt _ = Pumgrana.insert_links links in
  Lwt.return ()

lwt () = create_discography_links ids
