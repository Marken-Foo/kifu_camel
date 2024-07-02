open Kifu_camel

let sfen_ast = Alcotest.testable Sfen_parser.pp_sfen ( = )

let test_cases =
  [
    Alcotest.test_case "Starting position" `Quick (fun () ->
        let starting_sfen =
          "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
        in
        let result = Sfen_parser.parse starting_sfen in
        let make_row strs =
          Sfen_parser.Row
            (List.map
               (function "." -> Sfen_parser.EmptySq | s -> Sfen_parser.Koma s)
               strs)
        in
        let board =
          Sfen_parser.(
            Board
              [
                make_row [ "l"; "n"; "s"; "g"; "k"; "g"; "s"; "n"; "l" ];
                make_row [ "."; "r"; "."; "."; "."; "."; "."; "b"; "." ];
                make_row [ "p"; "p"; "p"; "p"; "p"; "p"; "p"; "p"; "p" ];
                make_row (List.init 9 (Fun.const "."));
                make_row (List.init 9 (Fun.const "."));
                make_row (List.init 9 (Fun.const "."));
                make_row [ "P"; "P"; "P"; "P"; "P"; "P"; "P"; "P"; "P" ];
                make_row [ "."; "B"; "."; "."; "."; "."; "."; "R"; "." ];
                make_row [ "L"; "N"; "S"; "G"; "K"; "G"; "S"; "N"; "L" ];
              ])
        in
        let expected : (Sfen_parser.parsed_sfen, string) result =
          Ok
            {
              board;
              side = Sfen_parser.Sente;
              hands = Sfen_parser.EmptyHands;
              move_num = Sfen_parser.MoveNum 1;
            }
        in
        Alcotest.(check (result sfen_ast string))
          "Starting position" expected result);
  ]

let test_list = ("SFEN parsing", test_cases)
