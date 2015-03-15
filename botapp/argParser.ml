type t =
  { mutable not_recursive : bool;
    mutable iteration_max : int;
    mutable max_deep : int;
    mutable verbose : bool;
    mutable quick : bool;
    mutable offline : bool }

exception Help
exception Invalid_Argument

let empty = { not_recursive = false;
              iteration_max = -1;
              max_deep = -1;            (* Not Implemented *)
              verbose = false;          (* Not Implemented *)
              quick = false;            (* Not Implemented *)
              offline = false }         (* Not Implemented *)

let get_arg name is_taken value =
  let fail () =
    let () = Printf.printf "\n%s : is taken argument\n\n" name in
    raise Invalid_Argument
  in
  match value, is_taken with
  | Some x, true  -> x
  | None,   true  -> fail ()
  | None,   false -> ""
  | Some x, false -> ""

let int_of_string name str =
  try int_of_string str
  with _ ->
    let () = Printf.printf "\n%s : is taken 'int' as argument\n\n" name in
    raise Invalid_Argument

let set_n n r p = r.not_recursive <- true
let set_i n r p = r.iteration_max <- int_of_string n p
let set_d n r p = r.max_deep <- int_of_string n p
let set_v n r p = r.verbose <- true
let set_q n r p = r.quick <- true
let set_o n r p = r.offline <- true
let act_h n r p = raise Help

let data =
  [["-n"; "--not-auto-feed"],   set_n,  false,  "Stop the auto-feed effect";
   ["-i"; "--iteration"],       set_i,  true,   "Limit the maximum iteration";
   ["-d"; "--deep"],            set_d,  true,   "Limit the maximum deep";
   ["-v"; "--verbose"],         set_v,  false,  "Verbose mode";
   ["-q"; "--quick"],           set_q,  false,  "Quick mode";
   ["-o"; "--offline"],         set_o,  false,  "Offline / From file mode";
   ["-h"; "--help"],            act_h,  false,  "Print the help"]

let rec min v str =
  if String.length str < v
  then min v (str ^ " ")
  else str

let help () =
  print_endline "########## HELP ##########";
  let print (strs, act, is_taken, description) =
    let str_of_option str option =
      let sep = if String.length str > 0 then ", " else "" in
      str ^ sep ^ option
    in
    let line = List.fold_left str_of_option "" strs in
    print_endline ((min 24 line) ^ "\t" ^ description)
  in
  List.iter print data;
  raise Help

let invalid_opt e =
  Printf.printf "\n%s: is not a valid option\n\n" e;
  try help ()
  with Help -> raise Invalid_Argument

let compare s1 s2 = String.compare s1 s2 = 0

let arg_number is_taken = if is_taken then 1 else 0

let rec apply e p options = function
  | [] -> invalid_opt e
  | (strs, action, is_taken, description)::next ->
    if List.exists (compare e) strs then
      try
        let () = action e options (get_arg e is_taken p) in
        1 + arg_number is_taken
      with Help -> help ()
    else apply e p options next

let is_option e = Char.compare (String.get e 0) '-' = 0

let get_next_element list =
  if List.length list = 0 then None else
    let e = List.hd list in
    if not (is_option e) then Some e else None

let get_options input_list =
  let options = empty in
  let rec getter blist = function
    | [] -> blist
    | e::t ->
      if not (is_option e)
      then getter (e::blist) t
      else
        let next_e = get_next_element t in
        let nb = apply e next_e options data in
        let next = if nb = 1 then t else List.tl t in
        getter blist next
  in
  let list = getter [] input_list in
  List.rev list, options
