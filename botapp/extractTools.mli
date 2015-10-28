(** {b Extract tools}  *)

(** [is_something_else uri]
    It is used for platform selection *)
val is_something_else : Ptype.uri -> bool

(** [contained_uris_of body]
    @return containted uris found in the given [body] *)
val contained_uris_of : string -> Ptype.uri list
