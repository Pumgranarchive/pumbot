(** {b Xtractor abastraction}  *)

type document = { title: string; body: string; summary: string; content: string }

(** [get uri]
    @return data getted from readability of the given [uri]
*)
val get : Ptype.uri ->
  (Utils.Content.t * Utils.Link.t list * Utils.Uri.t list) option Lwt.t

(** [get_body uri]
    @return the getted body (full text) of the given [uri] *)
val body_of : Ptype.uri -> string option Lwt.t

(** [get_data uri]
    @return the getted title, summary, body and content of the given [uri] *)
val data_of : Ptype.uri -> document option Lwt.t
