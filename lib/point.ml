open Core
open Sexplib.Std
open Ppx_compare_lib.Builtin

type point = int * int [@@deriving compare, eq, sexp]

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

module Point = struct
  type t = point [@@deriving compare, eq, sexp]
end

module Point_set = Set.Make (Point)
