(** {b Extract tools}  *)

(** html meta data  *)
type meta = { title: string; description: string }

(** [is_something_else uri]
  * It is used for platform selection
  *)
val is_something_else : Ptype.uri -> bool

(** [contained_uris_of_html uri body]
  * @uri is used to resolved relative url
  * @return containted uris found in the given [body]
  *)
val contained_uris_of_html : Ptype.uri -> string -> Ptype.uri list

(** [meta_of_html html]
 *  @return Parsed meta of given html if found otherwise return none
 *)
val meta_of_html : string -> meta option
