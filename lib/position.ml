type t = {
  board : Board.t;
  hand_sente : Hand.t;
  hand_gote : Hand.t;
  turn : Side.t;
  move_number : int;
}

let empty () =
  {
    board = Board.empty ();
    hand_sente = Hand.empty ();
    hand_gote = Hand.empty ();
    turn = Side.Sente;
    move_number = 1;
  }

let hand_of (side : Side.t) (pos : t) : Hand.t =
  match side with Side.Sente -> pos.hand_sente | Side.Gote -> pos.hand_gote

module Sfen = struct
  let validate_board_size (Sfen_parser.Board b) : bool =
    let num_rows = 9 in
    let num_cols = 9 in
    let is_row_length (i : int) (Sfen_parser.Row r) = i = List.length r in
    List.length b = num_rows && List.for_all (is_row_length num_cols) b

  let koma_of_square (sq : Sfen_parser.square) : (Koma.t, string) result =
    let side_of_kstr kstr =
      if kstr = String.lowercase_ascii kstr then Ok Side.Gote
      else if kstr = String.uppercase_ascii kstr then Ok Side.Sente
      else Error ("Cannot determine side of koma '" ^ kstr ^ "'")
    in
    let ktype_of_kstr kstr =
      match String.uppercase_ascii kstr with
      | "P" -> Ok Koma.Ktype.FU
      | "L" -> Ok Koma.Ktype.KY
      | "N" -> Ok Koma.Ktype.KE
      | "S" -> Ok Koma.Ktype.GI
      | "G" -> Ok Koma.Ktype.KI
      | "B" -> Ok Koma.Ktype.KA
      | "R" -> Ok Koma.Ktype.HI
      | "K" -> Ok Koma.Ktype.OU
      | s -> Error ("Cannot determine type of koma '" ^ s ^ "'")
    in
    let promoted_ktype_of_kstr kstr =
      match String.uppercase_ascii kstr with
      | "P" -> Ok Koma.Ktype.TO
      | "L" -> Ok Koma.Ktype.NY
      | "N" -> Ok Koma.Ktype.NK
      | "S" -> Ok Koma.Ktype.NG
      | "B" -> Ok Koma.Ktype.UM
      | "R" -> Ok Koma.Ktype.RY
      | s -> Error ("Cannot determine type of promoted koma '" ^ s ^ "'")
    in
    let ( let* ) = Result.bind in

    match sq with
    | Koma k ->
        let* side = side_of_kstr k in
        let* ktype = ktype_of_kstr k in
        Ok (Koma.make side ktype)
    | PromotedKoma k ->
        let* side = side_of_kstr k in
        let* ktype = promoted_ktype_of_kstr k in
        Ok (Koma.make side ktype)
    | EmptySq -> Error "Cannot make koma from empty square"

  let set_row (row_idx : int) (board : Board.t) (Row sfen_r : Sfen_parser.row) :
      Board.t =
    let row_of_idx i = i + 1 in
    let col_of_idx i = 9 - i in
    List.fold_left
      (fun (i, board_acc) sq ->
        let koma = koma_of_square sq in
        match koma with
        | Ok k ->
            ( i + 1,
              Board.set_koma board_acc
                (Square.of_coords (col_of_idx i) (row_of_idx row_idx))
                k )
        | Error _ -> (i + 1, board_acc))
      (0, board) sfen_r
    |> snd

  let set_board (Board sfen_b : Sfen_parser.board) : Board.t =
    List.fold_left
      (fun (i, board_acc) row -> (i + 1, set_row i board_acc row))
      (0, Board.empty ())
      sfen_b
    |> snd
end

let of_sfen (sfen : string) : (t, string) result =
  let ( let* ) = Result.bind in
  let* parsed = Sfen_parser.parse sfen in
  Ok
    {
      board = Sfen.set_board parsed.board;
      hand_sente = Hand.empty ();
      hand_gote = Hand.empty ();
      turn = Side.Sente;
      move_number = 0;
    }

(* let of_sfen (sfen : string) : (t, string) result =
   let parsed_sfen = Sfen_parser.parse sfen in *)
