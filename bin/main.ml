open Kifu_camel

let ktype = Koma.Ktype.KE
let initial_count = 3

let result =
  Hand.empty ()
  |> Hand.set_komatype_count ktype initial_count
  |> Hand.dec_komatype ktype
  |> Hand.get_komatype_count ktype

let expected = initial_count - 1
let () = Format.printf "ktype: %s\n" (Koma.Ktype.to_csa ktype)
let () = Format.printf "expected: %i\nresult: %i\n" expected result
(*
   let () =
     Format.printf "Valid hand ktypes: %s\n"
       (Hand.KomaSet.to_list Hand.valid_ktype
       |> List.map Koma.Ktype.to_csa |> String.concat ", ")

   let () =
     Format.printf "Is valid hand ktype: %b\n" (Hand.is_valid_komatype ktype)

   let () =
     Format.printf "Is variant comparison equal?? %i\n"
       (Koma.Ktype.compare Koma.Ktype.HI Koma.Ktype.HI)

   let () =
     Format.printf "Is valid hand ktype manual: %b\n"
       (Hand.KomaSet.mem Koma.Ktype.HI Hand.valid_ktype) *)
