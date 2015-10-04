(* Initialize the random *)
let _ = Random.self_init ()

(* Initialize the pumgrana api uri *)
let pumgrana_api_uri = Ptype.uri_of_string "http://127.0.0.1:8081/"
let () = Pumgrana_http.set_pumgrana_api_uri pumgrana_api_uri

module Token =
struct

  let get name =
    let ic = open_in name in
    try
      let token = input_line ic in
      let () = close_in ic in
      token
    with e ->
      close_in_noerr ic;
      raise Not_found

  let readability = get "readability.token"
  let opencalais = get "opencalais.token"

end

module Str =
struct

  include Str

  let regexps = List.map Str.regexp

  let contains regexp str =
    try ignore (Str.search_forward regexp str 0); true
    with Not_found -> false

  let exists regexps str =
    List.exists (fun r -> contains r str) regexps

  let limit str size =
    if (String.length str <= size) then str
    else String.sub str 0 size

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

module Tag =
struct

  (* (subject * mark) *)
  type t = (string * float)

  let make subject =
    let mark = Random.float 100. in
    (subject, mark)

  let makes subjects =
    List.map make subjects

  let subject (subject, mark) = subject

  (* Authorized subjects *)
  let regeps = Str.regexps ["film"; "actor"; "actress"; "author"; "director";
                            "producer"; "tv show"; "film-maker"]

  let is_authorized (subject, tag) = true (* Str.exists regeps subject *)


end

module Content =
struct

  (* (uri * title * summary * tag list) *)
  type t = (Ptype.uri * string * string * Tag.t list)

  let make uri title summary tags = (uri, title, summary, tags)

  let uri (uri, title, summary, tags) = uri
  let title (uri, title, summary, tags) = title
  let summary (uri, title, summary, tags) = summary
  let tags (uri, title, summary, tags) = tags
  let subjects (uri, title, summary, tags) = List.map Tag.subject tags

  let is_authorized (uri, title, summary, tags) =
    List.exists Tag.is_authorized tags

  let print (uri, title, summary, tags) =
      Printf.printf
        "uri\t%s\ntitle\t%s\nsummary\t%s\ntags\t%s\n\n"
        (Ptype.string_of_uri uri)
        title
        summary
        (Tag.subject (List.hd tags))

  let insert (uri, title, summary, tags) =
    print_endline "\nInsert Content ::";
    print (uri, title, summary, tags);
    Pumgrana_http.insert_content uri title summary tags

end

module LinkId =
struct

  type t = string

  let compare = String.compare

end


module Link =
struct

  (* (origin * target * nature * mark) *)
  type t = (Ptype.uri * Ptype.uri * string * float)

  let print links =
    let aux (o, t, nature, mark) =
      Printf.printf
        "origin\t%s\ntarget\t%s\nnature\t%s\nmark\t%f\n\n"
        (Ptype.string_of_uri o)
        (Ptype.string_of_uri t)
        nature
        mark
    in
    print_endline "";
    List.iter aux links;
    Printf.printf "%d links inserted\n\n" (List.length links)

  let map func list =
    Lwt_list.concat (Lwt_list.map_p func list)

  let origin (origin, target, nature, mark) = origin
  let target (origin, target, nature, mark) = target
  let id (origin, target, nature, mark) =
    (Ptype.string_of_uri origin) ^ "@" ^
      (Ptype.string_of_uri target)

  let nature (origin, target, nature, mark) = nature
  let mark (origin, target, nature, mark) = mark

  let build_inter_link n1 n2 l1 l2 =
    let dep2 e1 build e2 =
      let mark = Random.float 100. in
      if Ptype.compare_uri e1 e2 != 0
      then (e1, e2, n1, mark)::((e2, e1, n2, mark)::build)
      else build
    in
    let dep1 build e1 =
      List.fold_left (dep2 e1) build l2
    in
    List.fold_left dep1 [] l1

  let build_each_on_all nature list =
    let dep2 e1 build e2 =
      let mark = Random.float 100. in
      if Ptype.compare_uri e1 e2 != 0
      then (e1, e2, nature, mark)::build
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
