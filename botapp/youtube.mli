(** {b Youtube abastraction}  *)

(** [is_youtube_uri uri]
    It is used for platform selection *)
val is_youtube_uri : Ptype.uri -> bool

(** [search text]
    Search video on youtube with the given title + film
    The results are limited to 10 *)
val search : string -> Ptype.uri list Lwt.t

(** [get uri]
    @return data getted from youtube of the given [uri]
*)
val switch : Ptype.uri ->
  (Utils.Content.t * Utils.Link.t list * Utils.Uri.t list) option Lwt.t
