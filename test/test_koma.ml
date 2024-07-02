open Kifu_camel

let koma = Alcotest.testable Koma.pp ( = )

let test_promoted () =
  let result = Koma.is_promoted (Koma.make Side.Sente Koma.Ktype.FU) in
  let expected = false in
  Alcotest.(check bool) "Pawn is not promoted" expected result

let test_side () =
  Alcotest.(check (neg koma))
    "Sente pawn is not gote pawn"
    (Koma.make Side.Sente Koma.Ktype.FU)
    (Koma.make Side.Gote Koma.Ktype.FU)

let test_cases =
  [
    Alcotest.test_case "Pawn is not promoted" `Quick test_promoted;
    Alcotest.test_case "Sente pawn is not gote pawn" `Quick test_side;
  ]

let test_list = ("Koma", test_cases)
