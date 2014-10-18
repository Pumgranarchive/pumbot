module Lwt_list =
struct

  include Lwt_list

  let hd list =
    lwt l = list in
    Lwt.return (List.hd l)

  let concat lists =
    lwt ls = lists in
    Lwt.return (List.concat ls)

end


let (^.^) a b = a ^ " " ^ b

let print_link links =
  let print (o, t, tags) =
    Printf.printf
      "o:%s\nt:%s\ntags:%s\n\n"
      (Ptype.string_of_uri o)
      (Ptype.string_of_uri t)
      (Ptype.string_of_uri (List.hd tags))
  in
  print_endline "";
  List.iter print links;
  Printf.printf "%d links inserted\n\n" (List.length links)

let map_links func list =
  Lwt_list.concat (Lwt_list.map_p func list)
