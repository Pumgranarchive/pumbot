type t = {
  mutable api_host : Ptype.uri;
  mutable not_recursive : bool;
  mutable iteration_max : int;
  mutable max_deep : int;
  mutable verbose : bool
}

exception Help
exception Invalid_Argument

let default_option_values = {
  api_host = Ptype.uri_of_string "http://127.0.0.1:8081";
  not_recursive = false;
  iteration_max = 0;
  max_deep = 0;
  verbose = false               (* Not Implemented *)
}

(**
 * get_arg option_name is_taken_argument parameter_value
 * Try to get argument value (if necessary) or raise an exception
 *)
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

(**
  * Try to convert argument value as integer or raise an exception
  *)
let int_of_string name value =
  try int_of_string value
  with _ ->
    let () = Printf.printf "\n%s : is taken 'int' as argument\n\n" name in
    raise Invalid_Argument

(**
  * Try to convert argument value as uri or raise an exception
  *)
let uri_of_string name value =
  try Ptype.uri_of_string value
  with _ ->
    let () = Printf.printf "\n%s : is taken 'uri' as argument\n\n" name in
    raise Invalid_Argument

(** setter option_name record_values parameter_value *)
let set_a n r p = r.api_host <- uri_of_string n p
let set_n n r p = r.not_recursive <- true
let set_i n r p = r.iteration_max <- int_of_string n p
let set_d n r p = r.max_deep <- int_of_string n p
let set_v n r p = r.verbose <- true
let act_h n r p = raise Help

(** [short_name, long_name],    seter,  is taking argument,     description  *)
let option_settings =
  [["-a"; "--api-host"],        set_a,  true,   "Set the api host, default http://127.0.0.1:8081";
   ["-n"; "--not-auto-feed"],   set_n,  false,  "Stop the auto-feed effect";
   ["-i"; "--iteration"],       set_i,  true,   "Limit the maximum iteration";
   ["-d"; "--deep"],            set_d,  true,   "Limit the maximum deep";
   ["-v"; "--verbose"],         set_v,  false,  "Verbose mode";
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
  List.iter print option_settings;
  raise Help

let invalid_opt e =
  Printf.printf "\n%s: is not a valid option\n\n" e;
  try help ()
  with Help -> raise Invalid_Argument

let compare s1 s2 = String.compare s1 s2 = 0

let arg_number is_taken = if is_taken then 1 else 0

let rec apply option_name parameter options = function
  | [] -> invalid_opt option_name
  | (strs, action, is_taken, description)::next ->
    if List.exists (compare option_name) strs then
      try
        let value = get_arg option_name is_taken parameter in
        let () = action option_name options value in
        1 + arg_number is_taken
      with Help -> help ()
    else apply option_name parameter options next

let is_option e = Char.compare (String.get e 0) '-' = 0

let get_next_element list =
  if List.length list = 0 then None else
    let e = List.hd list in
    if not (is_option e) then Some e else None

let get_options input_list =
  let options = default_option_values in
  let rec getter blist = function
    | [] -> blist
    | e::t ->
      if not (is_option e)
      then getter (e::blist) t
      else
        let next_e = get_next_element t in
        let nb = apply e next_e options option_settings in
        let next = if nb = 1 then t else List.tl t in
        getter blist next
  in
  let list = getter [] input_list in
  List.rev list, options
