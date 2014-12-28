(** {b Youtube abastraction}  *)

(** [is_youtube_uri uri]
    It is used for platform selection *)
val is_youtube_uri : Ptype.uri -> bool

(** [get uri]
    @return data getted from youtube of the given [uri]
    formated as (tag_ids, links, uris)
*)
val switch : Ptype.uri ->
  (Ptype.uri list *
   (Rdf_uri.uri * Rdf_uri.uri * Ptype.uri list * int) list *
   Rdf_uri.uri list)
  Lwt.t
