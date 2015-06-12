(** {b Dbpedia abastraction}  *)

(** [is_wikipedia_uri uri]
    It is used for platform selection *)
val is_wikipedia_uri : Ptype.uri -> bool

(** [get uri]
    @return data getted from dbpedia of the given [uri]
    formated as (content, links, uris)
*)
val get : Ptype.uri ->
  ((Ptype.uri * string * string * string list) *
   (Rdf_uri.uri * Rdf_uri.uri * string * int) list *
   Rdf_uri.uri list)
  Lwt.t
