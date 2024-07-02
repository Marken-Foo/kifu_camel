open Kifu_camel

let is_hand_empty h =
  List.for_all
    (fun ktype -> Hand.get_komatype_count ktype h = 0)
    [
      Koma.Ktype.FU;
      Koma.Ktype.KY;
      Koma.Ktype.KE;
      Koma.Ktype.GI;
      Koma.Ktype.KI;
      Koma.Ktype.KA;
      Koma.Ktype.HI;
      Koma.Ktype.OU;
      Koma.Ktype.TO;
      Koma.Ktype.NY;
      Koma.Ktype.NK;
      Koma.Ktype.NG;
      Koma.Ktype.UM;
      Koma.Ktype.RY;
    ]

let test_cases =
  [
    Alcotest.test_case "Hand is empty" `Quick (fun () ->
        let result = Hand.empty () |> is_hand_empty in
        Alcotest.(check bool) "Empty hand" true result);
    Alcotest.test_case "Set hand count" `Quick (fun () ->
        let ktype = Koma.Ktype.GI in
        let count = 2 in
        let result =
          Hand.empty ()
          |> Hand.set_komatype_count ktype count
          |> Hand.get_komatype_count ktype
        in
        let expected = count in
        Alcotest.(check int) "Hand count" expected result);
    Alcotest.test_case "Reset hand" `Quick (fun () ->
        let ktype = Koma.Ktype.KI in
        let count = 3 in
        let result =
          Hand.empty ()
          |> Hand.set_komatype_count ktype count
          |> Hand.reset |> is_hand_empty
        in
        Alcotest.(check bool) "Empty hand" true result);
    Alcotest.test_case "Increment hand" `Quick (fun () ->
        let ktype = Koma.Ktype.KY in
        let initial_count = 2 in
        let result =
          Hand.empty ()
          |> Hand.set_komatype_count ktype initial_count
          |> Hand.inc_komatype ktype
          |> Hand.get_komatype_count ktype
        in
        let expected = initial_count + 1 in
        Alcotest.(check int) "Incremented hand count" expected result);
    Alcotest.test_case "Decrement hand" `Quick (fun () ->
        let ktype = Koma.Ktype.KE in
        let initial_count = 3 in
        let result =
          Hand.empty ()
          |> Hand.set_komatype_count ktype initial_count
          |> Hand.dec_komatype ktype
          |> Hand.get_komatype_count ktype
        in
        let expected = initial_count - 1 in
        Alcotest.(check int) "Decremented hand count" expected result);
    Alcotest.test_case "Decrement hand with zero pieces" `Quick (fun () ->
        let ktype = Koma.Ktype.KE in
        let result =
          Hand.empty () |> Hand.dec_komatype ktype
          |> Hand.get_komatype_count ktype
        in
        let expected = 0 in
        Alcotest.(check int)
          "Decremented hand count with zero pieces" expected result);
    Alcotest.test_case "Hand from list" `Quick (fun () ->
        let input =
          [ (Koma.Ktype.FU, 1); (Koma.Ktype.KE, 2); (Koma.Ktype.GI, 3) ]
        in
        let rest =
          [
            (Koma.Ktype.KY, 0);
            (Koma.Ktype.KI, 0);
            (Koma.Ktype.KA, 0);
            (Koma.Ktype.HI, 0);
          ]
        in
        let input = input @ rest in
        let hand = Hand.of_list input in
        let result =
          List.map (fun (ktype, _) -> Hand.get_komatype_count ktype hand) rest
        in
        let expected = List.map snd rest in
        Alcotest.(check (list int)) "Hand from list" expected result);
  ]

let test_list = ("Hand", test_cases)
