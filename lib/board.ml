(* Board is 9x9, represented by a flat array.
   Numbering is left-to-right, top-to-bottom.
   i.e. Square 91 is index 0, square 11 is index 8, square 26 is index 52, etc. *)
type t = Koma.t option array

let empty () = Array.make 81 (None : Koma.t option)
let sq_to_idx sq = match sq with Square.T (col, row) -> (9 * row) - col
(* (num_rows * (row - 1)) + (num_cols - col) *)

let get_koma (b : t) sq = b.(sq_to_idx sq)

let set_koma b sq (koma : Koma.t) =
  b.(sq_to_idx sq) <- Some koma;
  b

let rows_of (b : t) =
  let row_fold acc koma =
    match acc with
    | row, rows when List.length row = 9 -> ([ koma ], row :: rows)
    | row, rows -> (koma :: row, rows)
  in
  Array.fold_left row_fold ([], []) b |> snd |> List.rev

let to_string (b : t) =
  let print_board_koma k =
    match k with None -> " * " | Some koma -> Koma.to_csa koma
  in
  rows_of b
  |> List.map (fun row -> row |> List.map print_board_koma |> String.concat "")
  |> String.concat "\n"

let pp ppf (b : t) = Format.fprintf ppf "%s" (to_string b)
