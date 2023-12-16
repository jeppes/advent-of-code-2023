type point = int * int [@@deriving show]

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
