open Core
open Point

type node =
  | Rock
  | Empty
[@@deriving eq, compare, sexp]

let show_node node =
  match node with
  | Rock -> "#"
  | Empty -> "."
;;

let parse lines =
  let graph =
    Array.make_matrix
      ~dimy:(List.length lines)
      ~dimx:(String.length (List.hd_exn lines))
      Empty
  in
  let start = ref (0, 0) in
  for row = 0 to List.length lines - 1 do
    let line = List.nth_exn lines row in
    for col = 0 to String.length line - 1 do
      let c = String.get line col in
      let node =
        match c with
        | '.' -> Empty
        | '#' -> Rock
        | 'S' ->
          start := row, col;
          Empty
        | _ -> failwith "invalid char"
      in
      graph.(row).(col) <- node
    done
  done;
  graph, !start
;;

let make_cache row_count col_count goal =
  let cache = Array.make_matrix ~dimy:row_count ~dimx:col_count None in
  Array.map cache ~f:(fun row ->
    Array.map row ~f:(fun _ -> Array.create ~len:(goal + 1) false))
;;

let find_steps graph start goal =
  let row_count = Array.length graph in
  let col_count = Array.length graph.(0) in
  let neighbors_of point =
    neighbors_of_point_no_diagonals point
    |> List.filter ~f:(fun (row, col) ->
      row >= 0
      && col >= 0
      && row < row_count
      && col < col_count
      && not (equal_node graph.(row).(col) Rock))
  in
  let cache = make_cache row_count col_count goal in
  let rec find_steps_aux step point visited =
    let cache_value = cache.(fst point).(snd point).(step) in
    if cache_value
    then 0
    else if step = goal
    then (
      cache.(fst point).(snd point).(step) <- true;
      1 (* Set.count visited ~f:(fun _ -> true) *))
    else (
      let neighbors = neighbors_of point in
      let new_visited = Set.add visited point in
      let visited_count =
        List.fold_left neighbors ~init:0 ~f:(fun acc neighbor ->
          acc + find_steps_aux (step + 1) neighbor new_visited)
      in
      cache.(fst point).(snd point).(step) <- true;
      visited_count)
  in
  find_steps_aux 0 start Point_set.empty
;;

let solve_1 input =
  let graph, start = parse input in
  let solution = find_steps graph start 64 in
  solution
;;
