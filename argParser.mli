(* val u : unit *)

(** {b ArgParser parse and format argument} *)

(** The module type *)
type t =
  { mutable not_recursive : bool;
    mutable iteration_max : int;
    mutable max_deep : int;
    mutable verbose : bool;
    mutable quick : bool;
    mutable offline : bool }

(** Raise in case of help asking *)
exception Help

(** Raise in case of base usage  *)
exception Invalid_Argument

(** [get_options input_list]
    @param input_list should be Sys.argv as string list
    @return parameters which are not options + options themselves *)
val get_options : string list -> string list * t
