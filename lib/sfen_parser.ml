(* SFEN specification
   SFEN is a string that consists of:
   [board side_to_move hands move_number]
   Separated by whitespace.

   Board is a '/' separated string, with pieces and numbers (empty squares)
   Side to move is either 'b' or 'w'
   Hands are a series of characters, uppercase for sente, lowercase for gote.
   e.g. S2Pb3n is Sente: silver pawn pawn, gote: bishop knight knight knight
   Move number is an int.

   Some kind of grammar:

   Sfen = Board Side Hands Movenum
   Movenum = int
   Side = 'b' | 'w'
   Hands = '-' | Handpiece Handpieces
   Handpieces = e | Handpiece Handpieces
   Handpiece = Handpiecechar | int Handpiecechar
   Handpiecechar = (oneof) "PLNSGBRplnsgbr"
   Board = Row ('/' Row) * 8
   Row = int | Piecechar | '+' Piecechar
   Piecechar = (oneof) "PLNSGBRKplnsgbrk"
*)

open Angstrom

type square = EmptySq | Koma of string | PromotedKoma of string
type row = Row of square list
type board = Board of row list

type side = Sente | Gote
type koma_count = KomaCount of (char * int)
type hands = EmptyHands | SomeHands of koma_count list
type move_num = MoveNum of int

type parsed_sfen = {
  board : board;
  side : side;
  hands : hands;
  move_num : move_num;
}

let is_space_or_tab = function ' ' | '\t' -> true | _ -> false

let lookahead_space =
  peek_char >>= function
  | None -> fail "EOF?!"
  | Some c -> if is_space_or_tab c then peek_char else fail "keep going"

let lookahead_space_or_slash =
  peek_char >>= function
  | None -> fail "EOF?!"
  | Some c ->
      if is_space_or_tab c || (function '/' -> true | _ -> false) c then
        peek_char
      else fail "keep going"

let is_digit = function '0' .. '9' -> true | _ -> false

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let move_num =
  lift
    (function "" -> MoveNum 1 | s -> MoveNum (int_of_string s))
    (take_while is_digit)

let space_terminated p =
  let* v = p in
  let* _ = many1 (skip is_space_or_tab) in
  return @@ Fun.id v

let side =
  peek_char >>= function
  | None -> fail "Unexpected end of SFEN, expected side 'b' or 'w'"
  | Some 'b' ->
      let* _ = char 'b' in
      return @@ Fun.const Sente ()
  | Some 'w' ->
      let* _ = char 'w' in
      return @@ Fun.const Gote ()
  | Some c -> fail @@ Format.sprintf "Unknown side to move: '%c'" c

let hand_koma =
  peek_char >>= function
  | None -> fail "Unexpected end of SFEN while reading hand"
  | Some c when is_digit c ->
      let make_koma_count n c = KomaCount (c, int_of_string n) in
      let* i = take_while is_digit in
      let* k = satisfy is_letter in
      return @@ make_koma_count i k
  | Some c when is_letter c ->
      let* _ = take 1 in
      return @@ KomaCount (c, 1)
  | Some c -> fail @@ Format.sprintf "Unknown hand koma character '%c'" c

let hands =
  peek_char >>= function
  | None -> fail "Unexpected end of SFEN, expected hand information"
  | Some '-' ->
      let* _ = char '-' in
      return @@ EmptyHands
  | Some _ ->
      let* kcs = many_till hand_koma lookahead_space in
      return @@ SomeHands kcs

let row_char =
  peek_char >>= function
  | None -> fail "Unexpected end of SFEN while reading row"
  | Some c when is_digit c ->
      let empty_squares c = List.init (int_of_string c) (Fun.const EmptySq) in
      let* i = take_while is_digit in
      return @@ empty_squares i
  | Some '+' ->
      let make_promoted_koma c = [ PromotedKoma (String.make 1 c) ] in
      let* _ = char '+' in
      let* k = satisfy is_letter in
      return @@ make_promoted_koma k
  | Some _ ->
      let make_koma c = [ Koma (String.make 1 c) ] in
      let* k = satisfy is_letter in
      return @@ make_koma k

let row =
  let build_row (cs : square list list) = Row (List.concat cs) in
  let* cs = many_till row_char lookahead_space_or_slash in
  return @@ build_row cs

let board =
  let* rows = sep_by1 (char '/') row in
  return @@ (fun b -> Board b) rows

let sfen =
  let make_parsed_sfen board side hands move_num =
    { board; side; hands; move_num }
  in
  let* b = space_terminated board in
  let* s = space_terminated side in
  let* h = space_terminated hands in
  let* m = move_num in
  return @@ make_parsed_sfen b s h m

let parse = parse_string ~consume:Prefix sfen

let pp_sq ppf (sq : square) =
  Format.fprintf ppf "%s"
    (match sq with
    | EmptySq -> " . "
    | Koma s -> " " ^ s ^ " "
    | PromotedKoma s -> Format.sprintf "+%s " s)

let pp_row ppf (Row r : row) =
  Format.fprintf ppf "%a" (Format.pp_print_list pp_sq) r

let pp_board ppf (Board b : board) =
  Format.fprintf ppf "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_force_newline pp_row)
    b

let pp_side ppf (side : side) =
  Format.fprintf ppf "%s" (match side with Sente -> "sente" | Gote -> "gote")

let pp_koma_count ppf (KomaCount (k, n) : koma_count) =
  Format.fprintf ppf "%c: %i" k n

let pp_hands ppf (hands : hands) =
  match hands with
  | EmptyHands -> Format.fprintf ppf "-"
  | SomeHands kcs ->
      Format.fprintf ppf "%a" (Format.pp_print_list pp_koma_count) kcs

let pp_move_num ppf (MoveNum n : move_num) = Format.fprintf ppf "%i" n

let pp_sfen ppf (sfen : parsed_sfen) =
  Format.fprintf ppf "%s\nSide to move: %s\nMove number:%s\nHands: %s"
    (Format.asprintf "%a" pp_board sfen.board)
    (Format.asprintf "%a" pp_side sfen.side)
    (Format.asprintf "%a" pp_move_num sfen.move_num)
    (Format.asprintf "%a" pp_hands sfen.hands)
