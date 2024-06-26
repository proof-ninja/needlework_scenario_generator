type xml
type t = xml

val parse_file : string -> t
val children : t -> t list

val to_string : t -> string

val (//) : t -> string -> t
val (//?) : t -> string -> t option
val (/) : t -> string -> t list
val (/@) : t -> string -> string
val text : t -> string
