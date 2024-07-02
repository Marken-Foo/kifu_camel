open Kifu_camel

let board = Alcotest.testable Board.pp ( = )
let koma = Alcotest.testable Koma.pp ( = )

let test_cases =
  [
    Alcotest.test_case "Board is empty" `Quick (fun () ->
        let coords = List.init 81 (fun n -> (1 + (n / 9), 1 + (n mod 9))) in
        let b = Board.empty () in
        let is_sq_empty (col, row) =
          let sq = Square.of_coords col row in
          None = Board.get_koma b sq
        in
        let result = List.for_all is_sq_empty coords in
        Alcotest.(check bool) "Empty board" true result);
    Alcotest.test_case "Get empty square" `Quick (fun () ->
        let b = Board.empty () in
        let sq = Square.of_coords 1 1 in
        let result = Board.get_koma b sq in
        let expected = None in
        Alcotest.(check (option koma)) "Get empty square" expected result);
    Alcotest.test_case "Set gote bishop" `Quick (fun () ->
        let sq = Square.of_coords 5 4 in
        let k = Koma.make Side.Gote Koma.Ktype.KA in
        let b = Board.set_koma (Board.empty ()) sq k in
        let result = Board.get_koma b sq in
        let expected = Some k in
        Alcotest.(check (option koma)) "Set gote bishop" expected result);
  ]

let test_list = ("Board", test_cases)
