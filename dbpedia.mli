(** {b Dbpedia abastraction}  *)

(** [is_wikipedia_uri uri]
    It is used for platform selection *)
val is_wikipedia_uri : Ptype.uri -> bool

(** [get uri]
    @return data getted from dbpedia of the given [uri]
    formated as (tag_ids, links, uris)
*)
val get : Ptype.uri ->
  (Ptype.uri list *
   (Rdf_uri.uri * Rdf_uri.uri * Ptype.uri list * int) list *
   Rdf_uri.uri list)
  Lwt.t
