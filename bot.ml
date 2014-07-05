
  (* let uri = Ptype.uri_of_string "http://www.youtube.com/watch?v=DaFu3AoDXPY" in *)
  (* lwt contents = Pumgrana.get_contents () in *)

(* (\* init *\) *)
(* let _ = *)
(*   let server_uri = Ptype.uri_of_string "http://163.5.84.222/api/" in *)
(*   Pumgrana.set_pumgrana_api_uri server_uri *)

lwt _ =
  lwt tags_uri = Pumgrana.insert_tags Ptype.Link ["Talk about";"Mentioned by"]
  in
    (* [Ptype.uri_of_string "http://pumgrana.com/tag/link/Talk+about"; *)
    (*  Ptype.uri_of_string "http://pumgrana.com/tag/link/Mentioned+by"] in *)
  let wiki_links_from_video all_wiki_links video =
    let (_,_,url,_,categories) = video in
    lwt all_wiki_links = all_wiki_links in
    let get_links wiki_list topic =
      lwt l = wiki_list in
      lwt (_,_,_,_,_,wiki_urls) = Freebase_http.get_topics topic in
      let format wiki_url =
        let url = Ptype.uri_of_string url in
        let wiki_url = Ptype.uri_of_string wiki_url in
        [(url, wiki_url, [List.hd tags_uri]);(wiki_url, url, [List.nth tags_uri 1])]
      in
      let links = List.concat (List.map format wiki_urls) in
      Lwt.return (links@l)
    in
    let (topics, rel_topics) = categories in
    lwt wiki_links = List.fold_left get_links (Lwt.return []) topics in
    lwt rel_wiki_links = List.fold_left get_links (Lwt.return []) rel_topics in
    Lwt.return ((wiki_links@rel_wiki_links)@all_wiki_links)
  in

  let id = Youtube_http.get_id_from_url "https://www.youtube.com/watch?v=MZeh07V9vRo&list=TL7holR59tY4PTd7DruZVsGT7IzQ30N3A7" in

  lwt videos = Youtube_http.get_videos_from_ids [id] in
  (* List.iter Youtube_http.print_youtube_video videos; *)
  let print (o, t, tags) =
    Printf.printf
      "o:%s\nt:%s\ntags:%s\n\n"
      (Ptype.string_of_uri o)
      (Ptype.string_of_uri t)
      (Ptype.string_of_uri (List.hd tags))
  in
  lwt wiki_links = List.fold_left wiki_links_from_video (Lwt.return []) videos in
  List.iter print wiki_links;
  lwt _ = Pumgrana.insert_links wiki_links in
  Lwt.return ()
