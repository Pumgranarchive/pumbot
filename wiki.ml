open Utils

let from_video video =
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
