open Core
open Point

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving eq]

let show_direction direction =
  match direction with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
;;

let make_cache _ = ref String.Map.empty
let to_cache_key point direction = show_point point ^ "-" ^ show_direction direction
let point_from_cache_key key = String.split key ~on:'-' |> List.hd_exn |> read_point

let cache_put cache point direction value =
  let cache_key = to_cache_key point direction in
  cache := Map.add_exn !cache ~key:cache_key ~data:value
;;

let to_point_set set =
  set
  |> Set.to_list
  |> List.map ~f:point_from_cache_key
  |> List.map ~f:show_point
  |> String.Set.of_list
;;

let get_point_count_in_cache cache =
  !cache |> to_point_set |> Set.count ~f:(fun _ -> true)
;;

let cache_mem cache point direction = Map.mem !cache (to_cache_key point direction)

let cache_get_exn cache point direction =
  Map.find_exn !cache (to_cache_key point direction)
;;

type obstacle =
  | Empty
  | VerticalSplitter
  | HorizontalSplitter
  | UpCorner
  | DownCorner
[@@deriving eq, show]

let obstacle_of_char char =
  match char with
  | '.' -> Empty
  | '|' -> VerticalSplitter
  | '-' -> HorizontalSplitter
  | '/' -> UpCorner
  | '\\' -> DownCorner
  | _ -> failwith "invalid char"
;;

let obstacle_to_char obstacle =
  match obstacle with
  | Empty -> '.'
  | VerticalSplitter -> '|'
  | HorizontalSplitter -> '-'
  | UpCorner -> '/'
  | DownCorner -> '\\'
;;

let parse input =
  input
  |> List.map ~f:String.to_list
  |> List.map ~f:(fun line -> List.map line ~f:obstacle_of_char |> List.to_array)
  |> List.to_array
;;

let next_step direction =
  match direction with
  | Up -> -1, 0
  | Down -> 1, 0
  | Left -> 0, -1
  | Right -> 0, 1
;;

let turn_90_degrees_down direction =
  match direction with
  | Up -> Left
  | Down -> Right
  | Left -> Up
  | Right -> Down
;;

let turn_90_degrees_up direction =
  match direction with
  | Up -> Right
  | Down -> Left
  | Left -> Down
  | Right -> Up
;;

let point_add (row1, col1) (row2, col2) = row1 + row2, col1 + col2

let print_debug board _debug_points =
  let debug_points = to_point_set _debug_points |> Set.to_list in
  Array.iteri board ~f:(fun row line ->
    Array.iteri line ~f:(fun col obstacle ->
      let point = show_point (row, col) in
      if List.mem debug_points point ~equal:String.equal
      then print_string "X"
      else print_string (String.of_char (obstacle_to_char obstacle)));
    print_endline "")
;;

let solve board point direction cache =
  let obstacle_at (row, col) = board.(row).(col) in
  let rec visit point direction visited cache =
    let cache_key = to_cache_key point direction in
    let x, y = point in
    let out_of_bounds =
      x < 0 || x >= Array.length board || y < 0 || y >= Array.length board.(0)
    in
    if out_of_bounds
    then String.Set.empty
    else if cache_mem cache point direction
    then cache_get_exn cache point direction
    else if Set.mem visited cache_key
    then String.Set.singleton cache_key
    else (
      let new_visited = Set.add visited cache_key in
      let obstacle = obstacle_at point in
      let result =
        match obstacle, direction with
        | Empty, _ ->
          visit (point_add point (next_step direction)) direction new_visited cache
        | VerticalSplitter, direction
          when equal_direction direction Up || equal_direction direction Down ->
          visit (point_add point (next_step direction)) direction new_visited cache
        | HorizontalSplitter, direction
          when equal_direction direction Left || equal_direction direction Right ->
          visit (point_add point (next_step direction)) direction new_visited cache
        | VerticalSplitter, _ ->
          Set.union
            (visit (point_add point (next_step Up)) Up new_visited cache)
            (visit (point_add point (next_step Down)) Down new_visited cache)
        | HorizontalSplitter, _ ->
          Set.union
            (visit (point_add point (next_step Right)) Right new_visited cache)
            (visit (point_add point (next_step Left)) Left new_visited cache)
        | UpCorner, direction ->
          let new_direction = turn_90_degrees_up direction in
          visit
            (point_add point (next_step new_direction))
            new_direction
            new_visited
            cache
        | DownCorner, direction ->
          let new_direction = turn_90_degrees_down direction in
          visit
            (point_add point (next_step new_direction))
            new_direction
            new_visited
            cache
      in
      let result_set = Set.add result cache_key in
      cache_put cache point direction result_set;
      result_set)
  in
  visit point direction String.Set.empty cache
  |> to_point_set
  |> Set.count ~f:(fun _ -> true)
;;

let solve_1 input =
  let board = parse input in
  let cache = ref String.Map.empty in
  solve board (0, 0) Right cache
;;

let solve_2 input =
  let board = parse input in
  let row_count = Array.length board in
  let column_count = Array.length board.(0) in
  let outer_rim =
    [ (* top left *)
      [ (0, 0), Down ]
    ; [ (0, 0), Right ]
    ; (* top right *)
      [ (0, column_count - 1), Down ]
    ; [ (0, column_count - 1), Left ]
    ; (* bottom left *)
      [ (row_count - 1, 0), Up ]
    ; [ (row_count - 1, 0), Right ]
    ; (* bottom right *)
      [ (row_count - 1, column_count - 1), Up ]
    ; [ (row_count - 1, column_count - 1), Left ]
    ; (* Top row *)
      List.range 1 (column_count - 1) |> List.map ~f:(fun col -> (0, col), Down)
    ; (* Bottom row *)
      List.range 1 (column_count - 1) |> List.map ~f:(fun col -> (row_count - 1, col), Up)
    ; (* Left column *)
      List.range 1 (row_count - 1) |> List.map ~f:(fun row -> (row, 0), Right)
    ; (* Right column *)
      List.range 1 (row_count - 1)
      |> List.map ~f:(fun row -> (row, column_count - 1), Left)
    ]
    |> List.concat
  in
  List.map outer_rim ~f:(fun (point, direction) ->
    let cache = make_cache board in
    let count = solve board point direction cache in
    count)
  |> Util.list_max
;;
