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

val to_string : game_termination -> string

type t

val make : Side.t -> Square.t -> Square.t -> Koma.t -> Koma.t option -> t

val make_promotion :
  Side.t -> Square.t -> Square.t -> Koma.t -> Koma.t option -> t

val make_drop : Side.t -> Square.t -> Koma.t -> t
val make_termination : game_termination -> t
val to_string : t -> string
