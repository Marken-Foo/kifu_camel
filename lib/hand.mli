type t

val is_valid_komatype : Koma.Ktype.t -> bool
val empty : unit -> t
val reset : t -> t
val get_komatype_count : Koma.Ktype.t -> t -> int
val set_komatype_count : Koma.Ktype.t -> int -> t -> t
val inc_komatype : Koma.Ktype.t -> t -> t
val dec_komatype : Koma.Ktype.t -> t -> t
val of_list : (Koma.Ktype.t * int) list -> t
