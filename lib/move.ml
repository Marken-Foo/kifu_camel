type game_termination =
  | Abort
  | Resign
  | Impasse
  | Repetition
  | Mate
  | Flag
  | IllegalWin
  | IllegalLoss
  | EnteringKing

let to_string gt =
  match gt with
  | Abort -> "中断"
  | Resign -> "投了"
  | Impasse -> "持将棋"
  | Repetition -> "千日手"
  | Mate -> "詰み"
  | Flag -> "切れ負け"
  | IllegalWin -> "反則勝ち"
  | IllegalLoss -> "反則負け"
  | EnteringKing -> "入玉勝ち"

type t =
  | Normal of {
      side : Side.t;
      start_sq : Square.t;
      end_sq : Square.t;
      koma : Koma.t;
      captured : Koma.t option;
    }
  | Promotion of {
      side : Side.t;
      start_sq : Square.t;
      end_sq : Square.t;
      koma : Koma.t;
      captured : Koma.t option;
    }
  | Drop of { side : Side.t; end_sq : Square.t; koma : Koma.t }
  | Termination of game_termination

let make side start_sq end_sq koma captured =
  Normal { side; start_sq; end_sq; koma; captured }

let make_promotion side start_sq end_sq koma captured =
  Promotion { side; start_sq; end_sq; koma; captured }

let make_drop side end_sq koma = Drop { side; end_sq; koma }
let make_termination gt = Termination gt

let to_string = function
  | Normal { side = _; start_sq; end_sq; koma; captured = _ } ->
      Printf.sprintf "%s%s-%s" (Koma.to_csa koma)
        (Square.to_string start_sq)
        (Square.to_string end_sq)
  | Promotion { side = _; start_sq; end_sq; koma; captured = _ } ->
      Printf.sprintf "%s%s-%s+" (Koma.to_csa koma)
        (Square.to_string start_sq)
        (Square.to_string end_sq)
  | Drop { side = _; end_sq; koma } ->
      Printf.sprintf "%s*%s" (Koma.to_csa koma) (Square.to_string end_sq)
  | Termination gt -> to_string gt
