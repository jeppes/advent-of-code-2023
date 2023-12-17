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

let all_directions = [ Up; Right; Down; Left ]

let direction_index direction =
  List.findi_exn all_directions ~f:(fun _ d -> equal_direction direction d) |> fst
;;

type cache = Core.String.Set.t array array array

let make_cache board : cache =
  let row_count = Array.length board in
  let col_count = Array.length board.(0) in
  let make_directions_array _ =
    Array.create ~len:(List.length all_directions) String.Set.empty
  in
  let make_row_array _ =
    Array.create ~len:col_count 1 |> Array.map ~f:make_directions_array
  in
  let cache = Array.create ~len:row_count 1 |> Array.map ~f:make_row_array in
  cache
;;

let to_cache_key point direction = show_point point ^ "-" ^ show_direction direction
let point_from_cache_key key = String.split key ~on:'-' |> List.hd_exn |> read_point

let cache_put cache point direction value =
  cache.(fst point).(snd point).(direction_index direction) <- value
;;

let to_point_set set =
  set
  |> Set.to_list
  |> List.map ~f:point_from_cache_key
  |> List.map ~f:show_point
  |> String.Set.of_list
;;

let get_point_count_in_cache cache =
  let sum = ref 0 in
  for i = 0 to Array.length cache - 1 do
    for j = 0 to Array.length cache.(0) - 1 do
      let visited = ref false in
      for k = 0 to Array.length cache.(0).(0) - 1 do
        let value = cache.(i).(j).(k) in
        if not (Set.is_empty value) then visited := true
      done;
      if !visited then sum := !sum + 1
    done
  done;
  !sum
;;

let cache_mem (cache : cache) point direction =
  not (Set.is_empty cache.(fst point).(snd point).(direction_index direction))
;;

let cache_get_exn (cache : cache) point direction =
  cache.(fst point).(snd point).(direction_index direction)
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

let solve board point direction (cache : cache) =
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
  let cache = make_cache board in
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
  let cache = make_cache board in
  List.map outer_rim ~f:(fun (point, direction) ->
    let count = solve board point direction cache in
    count)
  |> Util.list_max
;;
