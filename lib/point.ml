open Core
open Sexplib.Std
open Ppx_compare_lib.Builtin

type point = int * int [@@deriving compare, eq, sexp, hash]

let show_point (x, y) = Printf.sprintf "(%d,%d)" x y
let read_point s = Scanf.sscanf s "(%d,%d)" (fun x y -> x, y)

let neighbors_of_point point =
  let row = fst point in
  let column = snd point in
  [ (*  Row above *)
    row - 1, column - 1
  ; row - 1, column
  ; row - 1, column + 1
  ; (* Current row *)
    row, column - 1
  ; row, column + 1
  ; (* Row below *)
    row + 1, column - 1
  ; row + 1, column
  ; row + 1, column + 1
  ]
;;

let neighbors_of_point_no_diagonals point =
  let row = fst point in
  let column = snd point in
  [ (*  Row above *)
    row - 1, column
  ; (* Current row *)
    row, column - 1
  ; row, column + 1
  ; (* Row below *)
    row + 1, column
  ]
;;

module Point = struct
  type t = point [@@deriving compare, eq, sexp, hash]
end

module Point_set = Set.Make (Point) [@@deriving compare, eq, sexp, hash]
