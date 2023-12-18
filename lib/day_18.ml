open Core

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving show]

type instruction =
  { direction : direction
  ; count : int
  }
[@@deriving show]

let find_points instructions =
  let rec find_points_aux instructions points =
    let row, col = List.hd points |> Option.value ~default:(0, 0) in
    match instructions with
    | { count; direction } :: rest ->
      (match direction with
       | Up -> find_points_aux rest ((row + count, col) :: points)
       | Down -> find_points_aux rest ((row - count, col) :: points)
       | Left -> find_points_aux rest ((row, col - count) :: points)
       | Right -> find_points_aux rest ((row, col + count) :: points))
    | [] -> List.rev points
  in
  find_points_aux instructions []
;;

(* Shoelace algorithm to find the area of the points
   then pick's theorem to get the number of points *)
let area parsed =
  let points = find_points parsed in
  let outer_points = List.map parsed ~f:(fun entry -> entry.count) |> Util.sum in
  let xs = List.map points ~f:fst in
  let ys = List.map points ~f:snd in
  let xs_shifted = List.tl_exn xs @ [ List.hd_exn xs ] in
  let ys_shifted = List.tl_exn ys @ [ List.hd_exn ys ] in
  let sum =
    List.zip_exn (List.zip_exn xs ys_shifted) (List.zip_exn xs_shifted ys)
    |> List.map ~f:(fun ((x1, y2), (x2, y1)) -> (x1 * y2) - (x2 * y1))
    |> Util.sum
  in
  let inner_area = abs sum / 2 in
  let inner_points = inner_area - (outer_points / 2) + 1 in
  inner_points + outer_points
;;

let solve_1 input =
  let parse input =
    List.map input ~f:(fun x ->
      let split = String.split x ~on:' ' in
      let direction =
        match List.hd_exn split with
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right
        | _ -> failwith "Invalid direction"
      in
      let count = Int.of_string (List.nth_exn split 1) in
      { direction; count })
  in
  area (parse input)
;;

let solve_2 input =
  let parse input =
    List.concat_map input ~f:(fun x ->
      let split = String.split x ~on:' ' in
      let hex_part = String.slice (List.nth_exn split 2) 1 8 in
      let hex_count = String.slice hex_part 1 6 |> Util.hex_to_int in
      let hex_direction' = String.slice hex_part 6 7 in
      let hex_direction =
        match hex_direction' with
        | "0" -> Right
        | "1" -> Down
        | "2" -> Left
        | "3" -> Up
        | _ -> failwith "Invalid hex direction"
      in
      [ { direction = hex_direction; count = hex_count } ])
  in
  area (parse input)
;;
