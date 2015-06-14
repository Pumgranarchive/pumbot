(** {b Dbpedia abastraction}  *)

(** [is_wikipedia_uri uri]
    It is used for platform selection *)
val is_wikipedia_uri : Ptype.uri -> bool

(** [get uri]
    @return data getted from dbpedia of the given [uri]
*)
val get : Ptype.uri ->
  (Utils.Content.t * Utils.Link.t list * Utils.Uri.t list) Lwt.t
