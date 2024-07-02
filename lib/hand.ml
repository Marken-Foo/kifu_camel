(* Represents the hand pieces of one player. *)

type t = (Koma.Ktype.t, int) Hashtbl.t

module KomaSet = Set.Make (Koma.Ktype)

let valid_ktype =
  KomaSet.of_list
    [
      Koma.Ktype.FU;
      Koma.Ktype.KY;
      Koma.Ktype.KE;
      Koma.Ktype.GI;
      Koma.Ktype.KI;
      Koma.Ktype.KA;
      Koma.Ktype.HI;
    ]

let is_valid_komatype ktype = KomaSet.mem ktype valid_ktype
let empty () : t = Hashtbl.create 7

let reset (hand : t) =
  Hashtbl.reset hand;
  hand

let get_komatype_count ktype (hand : t) =
  Hashtbl.find_opt hand ktype |> Option.value ~default:0

let set_komatype_count ktype count (hand : t) =
  if is_valid_komatype ktype then Hashtbl.replace hand ktype count else ();
  hand

let inc_komatype ktype (hand : t) =
  let count = get_komatype_count ktype hand in
  set_komatype_count ktype (count + 1) hand

let dec_komatype ktype (hand : t) =
  let count = get_komatype_count ktype hand in
  if count > 0 then set_komatype_count ktype (count - 1) hand else hand

let of_list ktype_pairs =
  let hand = empty () in
  List.iter (fun (ktype, count) -> Hashtbl.replace hand ktype count) ktype_pairs;
  hand
