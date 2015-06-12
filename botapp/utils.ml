(* Initialize the random *)
let _ = Random.self_init ()

module Str =
struct

  include Str

  let regexps = List.map Str.regexp

  let contains regexp str =
    try ignore (Str.search_forward regexp str 0); true
    with Not_found -> false

  let exists regexps str =
    List.exists (fun r -> contains r str) regexps

end

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

  let merge func l1 l2 =
    let add list e =
      if List.exists (func e) list
      then list
      else e::list
    in
    List.fold_left add l1 l2

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

module Uri =
struct

  type t = Ptype.uri
  let compare = Ptype.compare_uri

end

module Content =
struct

  let uri (uri, title, summary, subjects) = uri
  let title (uri, title, summary, subjects) = title
  let summary (uri, title, summary, subjects) = summary
  let subjects (uri, title, summary, subjects) = subjects

  let insert (uri, title, summary, subjects) =
    Pumgrana_http.insert_content uri title summary subjects

end

module Link =
struct

  type t = string

  let compare = String.compare

  let print links =
    let aux (o, t, subject, score) =
      Printf.printf
        "origin\t%s\ntarget\t%s\ntags\t%s\nscore\t%d\n\n"
        (Ptype.string_of_uri o)
        (Ptype.string_of_uri t)
        subject
        score
    in
    print_endline "";
    List.iter aux links;
    Printf.printf "%d links inserted\n\n" (List.length links)

  let map func list =
    Lwt_list.concat (Lwt_list.map_p func list)

  let origin (origin, target, nature, score) = origin
  let target (origin, target, nature, score) = target
  let id (origin, target, nature, score) =
    (Ptype.string_of_uri origin) ^ "@" ^
      (Ptype.string_of_uri target)

  let nature (origin, target, nature, score) = nature
  let score (origin, target, nature, score) = score

  let build_inter_link n1 n2 l1 l2 =
    let dep2 e1 build e2 =
      let score = Random.int 100 in
      if Ptype.compare_uri e1 e2 != 0
      then (e1, e2, n1, score)::((e2, e1, n2, score)::build)
      else build
    in
    let dep1 build e1 =
      List.fold_left (dep2 e1) build l2
    in
    List.fold_left dep1 [] l1

  let build_each_on_all nature list =
    let dep2 e1 build e2 =
      let score = Random.int 100 in
      if Ptype.compare_uri e1 e2 != 0
      then (e1, e2, nature, score)::build
      else build
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
