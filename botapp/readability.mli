(** {b Readability abastraction}  *)

(** [is_something_else uri]
    It is used for platform selection *)
val is_something_else : Ptype.uri -> bool

(** [get uri]
    @return data getted from readability of the given [uri]
    formated as (tag_ids, links, uris)
*)
val get : Ptype.uri ->
  (Ptype.uri list *
   (Rdf_uri.uri * Rdf_uri.uri * Ptype.uri list * int) list *
   Rdf_uri.uri list)
  Lwt.t

(** [get_body uri]
    @return the getted body of the given [uri] *)
val get_body : Ptype.uri -> string Lwt.t

(** [get_data uri]
    @return the getted title and body of the given [uri] *)
val get_data : Ptype.uri -> (string * string) Lwt.t

(** [get_contained_uris body]
    @return containted uris found in the given [body] *)
val get_contained_uris : string -> Ptype.uri list