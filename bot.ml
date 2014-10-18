type mode = Wiki | Discography


let mode = Discography

let create_links = match mode with
  | Wiki        -> Wiki.from_video
  | Discography -> Discography.from_video


let main () =
  let ids = List.map Youtube_http.get_video_id_from_url
    (List.tl (Array.to_list Sys.argv))
  in
  lwt videos = Youtube_http.get_videos_from_ids ids in
  lwt links = Utils.map_links create_links videos in
  let () = Utils.print_link links in
  lwt _ = Pumgrana.insert_links links in
  Lwt.return ()

lwt () = main ()
