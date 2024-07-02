let () =
  Alcotest.run "Shogi"
    [
      Test_koma.test_list;
      Test_board.test_list;
      Test_hand.test_list;
      Test_sfen.test_list;
    ]
