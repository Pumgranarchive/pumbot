module List =
struct

  include List

  let to_queue list =
    let q = Queue.create () in
    let () = List.iter (fun x -> Queue.add q x) list in
    q

  let concat_d2 lists =
    concat (concat lists)

  let concat_d3 lists =
    concat (concat (concat lists))

  let limit size l =
    let rec aux bl s = function
      | []   -> bl
      | h::t ->
        if s >= size then bl else
          aux (h::bl) (s + 1) t
    in
    List.rev (aux [] 0 l)

end

module Magic_queue =
struct

  exception Empty

  include List

  let from_list l = l

  let push e q = q@[e]

  let pop = function
    | h::t -> h, t
    | []   -> raise Empty

  let is_empty = function
    | [] -> true
    | _  -> false

end

module Lwt_list =
struct

  include Lwt_list

  let hd list =
    lwt l = list in
    Lwt.return (List.hd l)

  let concat lists =
    lwt ls = lists in
    Lwt.return (List.concat ls)

  let fold_left func initial list =
    let rec aux data = function
      | []   -> Lwt.return data
      | h::t ->
          lwt data' = func data h in
          aux data' t
    in
    aux initial list

  let dep_fold_left func initial dep_list =
    fold_left (fold_left func) initial dep_list

end

module Lwt_magic_queue =
struct

  let fold_left func limit init queue =
    let print e h =
      print_endline ((Ptype.string_of_uri h) ^ " : " ^ (Printexc.to_string e))
    in
    let rec aux data i = function
      | _ when limit >= 0 && i >= limit -> Lwt.return data
      | []   -> Lwt.return data
      | h::q ->
        lwt d', q' =
          try_lwt func data q h
          with e -> (print e h; Lwt.return (data, q))
        in
        aux d' (i + 1) q'
    in
    aux init 0 queue

end

module Tag =
struct

  module Of_Link =
  struct
    let make subject =
      Lwt_list.hd (Pumgrana_http.insert_tags Ptype.Link [subject])
    let makes subjects =
      Pumgrana_http.insert_tags Ptype.Link subjects
  end

  module Of_Content =
  struct
    let make subject =
      Lwt_list.hd (Pumgrana_http.insert_tags Ptype.Content [subject])

    let makes subjects =
      Pumgrana_http.insert_tags Ptype.Content subjects

    let assign tags_id content_uri =
      Pumgrana_http.update_content_tags content_uri tags_id
  end

end

module Link =
struct

  let print links =
    let aux (o, t, tags) =
      Printf.printf
        "o:%s\nt:%s\ntags:%s\n\n"
        (Ptype.string_of_uri o)
        (Ptype.string_of_uri t)
        (Ptype.string_of_uri (List.hd tags))
    in
    print_endline "";
    List.iter aux links;
    Printf.printf "%d links inserted\n\n" (List.length links)

  let map func list =
    Lwt_list.concat (Lwt_list.map_p func list)

  let build_inter_link t1 t2 l1 l2 =
    let dep2 e1 build e2 =
      (e1, e2, t1)::((e2, e1, t2)::build)
    in
    let dep1 build e1 =
      List.fold_left (dep2 e1) build l2
    in
    List.fold_left dep1 [] l1

  let build_each_on_all tag list =
    let dep2 e1 build e2 =
      if Ptype.compare_uri e1 e2 = 0
      then build
      else (e1, e2, tag)::build
    in
    let dep1 build e1 =
      List.fold_left (dep2 e1) build list
    in
    List.fold_left dep1 [] list

  let insert links =
    let () = print links in
    Pumgrana_http.insert_links links

end

let (^.^) a b = a ^ " " ^ b
