type t

val empty : unit -> t
val get_koma : t -> Square.t -> Koma.t option
val set_koma : t -> Square.t -> Koma.t -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
