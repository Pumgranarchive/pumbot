type t =
  { mutable not_recursive : bool;
    mutable verbose : bool;
    mutable quick : bool;
    mutable offline : bool }

exception Help
exception Invalid_Argument

let empty = { not_recursive = false;
              verbose = false;          (* Not Implemented *)
              quick = false;            (* Not Implemented *)
              offline = false }         (* Not Implemented *)

let set_n x = x.not_recursive <- true
let set_v x = x.verbose <- true
let set_q x = x.quick <- true
let set_o x = x.offline <- true
let act_h _ = raise Help

let data =
  [["-n"; "--not-recursive"],   set_n,  "Stop the recursive / auto-feed effect";
   ["-v"; "--verbose"],         set_v,  "Verbose mode";
   ["-q"; "--quick"],           set_q,  "Quick mode";
   ["-o"; "--offline"],         set_o,  "Offline / From file mode";
   ["-h"; "--help"],            act_h,  "Print the help"]

let rec min v str =
  if String.length str < v
  then min v (str ^ " ")
  else str

let help () =
  print_endline "########## HELP ##########";
  let print (strs, act, description) =
    let str_of_option str option =
      let sep = if String.length str > 0 then ", " else "" in
      str ^ sep ^ option
    in
    let line = List.fold_left str_of_option "" strs in
    print_endline ((min 24 line) ^ "\t" ^ description)
  in
  List.iter print data;
  raise Help

let invalid e =
  Printf.printf "\n%s: is not a valid option\n\n" e;
  try help ()
  with Help -> raise Invalid_Argument

let rec apply e options = function
  | (strs, action, description)::next ->
    if List.exists (fun s -> String.compare s e = 0) strs
    then try action options
      with Help -> help ()
    else apply e options next
  | [] -> invalid e

let get_options input_list =
  let options = empty in
  let getter blist e =
    if Char.compare (String.get e 0) '-' = 0
    then (apply e options data; blist)
    else e::blist
  in
  let list = List.fold_left getter [] input_list in
  List.rev list, options
