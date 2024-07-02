module Ktype : sig
  type t = FU | KY | KE | GI | KI | KA | HI | OU | TO | NY | NK | NG | UM | RY

  val to_csa : t -> string
  val is_promoted : t -> bool
  val compare : t -> t -> int
end

type t

val make : Side.t -> Ktype.t -> t
val to_csa : t -> string
val is_promoted : t -> bool
val pp : Format.formatter -> t -> unit
