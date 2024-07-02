type t = T of int * int (* (col, row) *)

let of_coords col row =
  if col < 1 || col > 9 || row < 1 || row > 9 then
    raise (Invalid_argument "Square out of range");
  T (col, row)

let to_string = function T (col, row) -> string_of_int col ^ string_of_int row
