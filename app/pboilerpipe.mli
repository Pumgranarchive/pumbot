(** {b Boilerpipe abastraction}  *)

(** [get uri]
    @return data getted from readability of the given [uri]
*)
val get : Ptype.uri ->
  (Utils.Content.t * Utils.Link.t list * Utils.Uri.t list) Lwt.t

(** [get_body uri]
    @return the getted body of the given [uri] *)
val body_of : Ptype.uri -> string Lwt.t

(** [get_data uri]
    @return the getted title, summary and body of the given [uri] *)
val data_of : Ptype.uri -> (string * string * string) Lwt.t
