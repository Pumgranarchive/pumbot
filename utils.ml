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

end

module Lwt_magic_queue =
struct

  let fold_left func init queue =
    let rec aux data = function
      | []   -> Lwt.return data
      | h::q ->
        lwt d', q' = func data q h in
        aux d' q'
    in
    aux init queue

end

module Tag =
struct

  module Of_Link =
  struct
    let make subject =
      Lwt_list.hd (Pumgrana.insert_tags Ptype.Link [subject])
    let makes subjects =
      Pumgrana.insert_tags Ptype.Link subjects
  end

  module Of_Content =
  struct
    let make subject =
      Lwt_list.hd (Pumgrana.insert_tags Ptype.Content [subject])
    let makes subjects =
      Pumgrana.insert_tags Ptype.Content subjects
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

end

let (^.^) a b = a ^ " " ^ b
