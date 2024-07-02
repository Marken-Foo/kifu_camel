module Ktype = struct
  type t = FU | KY | KE | GI | KI | KA | HI | OU | TO | NY | NK | NG | UM | RY

  let compare = compare

  let to_csa (ktype : t) =
    match ktype with
    | FU -> "FU"
    | KY -> "KY"
    | KE -> "KE"
    | GI -> "GI"
    | KI -> "KI"
    | KA -> "KA"
    | HI -> "HI"
    | OU -> "OU"
    | TO -> "TO"
    | NY -> "NY"
    | NK -> "NK"
    | NG -> "NG"
    | UM -> "UM"
    | RY -> "RY"

  let is_promoted = function
    | FU | KY | KE | GI | KI | KA | HI | OU -> false
    | TO | NY | NK | NG | UM | RY -> true
end

type t = Side.t * Ktype.t

let make side ktype = (side, ktype)

let to_csa = function
  | side, ktype ->
      let side_str = match side with Side.Sente -> "+" | Side.Gote -> "-" in
      side_str ^ Ktype.to_csa ktype

let is_promoted = function _, ktype -> Ktype.is_promoted ktype
let pp ppf (k : t) = Format.fprintf ppf "%s" (to_csa k)
