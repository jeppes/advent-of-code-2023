open Core
open Point

let parse input =
  List.map input ~f:(fun line ->
    let items = String.to_list line |> List.map ~f:String.of_char in
    let numbers = List.map items ~f:int_of_string in
    Array.of_list numbers)
  |> Array.of_list
;;

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving eq, show, compare, sexp]

let step_towards (row, col) direction =
  match direction with
  | Up -> row - 1, col
  | Down -> row + 1, col
  | Left -> row, col - 1
  | Right -> row, col + 1
;;

let is_valid_point city (row, col) =
  let max_row = Array.length city in
  let max_col = Array.length city.(0) in
  row >= 0 && row < max_row && col >= 0 && col < max_col
;;

let possible_directions city current_direction point steps =
  let potential_steps =
    match current_direction with
    | Up -> [ Up; Left; Right ]
    | Down -> [ Down; Left; Right ]
    | Left -> [ Left; Up; Down ]
    | Right -> [ Right; Up; Down ]
  in
  potential_steps
  |> List.filter ~f:(fun direction ->
    if equal_direction direction current_direction && steps > 2 then false else true)
  |> List.map ~f:(fun direction ->
    let next_steps =
      if equal_direction direction current_direction then steps + 1 else 1
    in
    step_towards point direction, direction, next_steps)
  |> List.filter ~f:(fun (point, _, _) -> is_valid_point city point)
;;

let to_visited_key steps current_direction point =
  string_of_int steps ^ "-" ^ show_direction current_direction ^ "-" ^ show_point point
;;

let point_from_visited_key key =
  let parts = String.split ~on:'-' key in
  List.nth_exn parts 2 |> read_point
;;

type key =
  { point : int * int
  ; direction : direction
  ; steps : int
  }
[@@deriving compare, sexp, eq]

type entry =
  { distance : int
  ; key : key
  }
[@@deriving compare, sexp, eq]

module SetPoint = struct
  type t = entry

  let sexp_of_t = sexp_of_entry
  let compare a b = compare_entry a b
  let t_of_sexp = entry_of_sexp
end

module PointSet = Set.Make (SetPoint)

let neighbors city current_direction point steps : key list =
  let potential_steps =
    match current_direction with
    | Up -> [ Up; Left; Right ]
    | Down -> [ Down; Left; Right ]
    | Left -> [ Left; Up; Down ]
    | Right -> [ Right; Up; Down ]
  in
  potential_steps
  |> List.filter ~f:(fun direction ->
    if equal_direction direction current_direction && steps > 2 then false else true)
  |> List.map ~f:(fun direction ->
    let next_steps =
      if equal_direction direction current_direction then steps + 1 else 1
    in
    let new_point = step_towards point direction in
    if equal_point new_point point then failwith "same point";
    { point = new_point; direction; steps = next_steps })
  |> List.filter ~f:(fun n -> is_valid_point city n.point)
;;

let to_cache_key steps current_direction point =
  string_of_int steps ^ "-" ^ show_direction current_direction ^ "-" ^ show_point point
;;

(*
   Original dijkstra's implementation in ocaml heavily inspired by:
   https://github.com/nilehmann/ocaml-algorithms/blob/master/dijkstra.ml
*)
let dijkstras (graph : int array array) =
  let distance = ref String.Map.empty in
  let write_distance (key : key) value =
    let cache_key = to_cache_key key.steps key.direction key.point in
    distance := Map.set !distance ~key:cache_key ~data:value
  in
  let read_distance (key : key) : int =
    let key = to_cache_key key.steps key.direction key.point in
    let map = !distance in
    let value = Map.find map key in
    Option.value value ~default:Int.max_value_30_bits
  in
  let neighbors_of n = neighbors graph n.direction n.point n.steps in
  let start = { distance = 0; key = { point = 0, 0; direction = Right; steps = 1 } } in
  let queue =
    ref
      (PointSet.of_list
         [ { start with key = { start.key with direction = Right } }
         ; { start with key = { start.key with direction = Down } }
         ])
  in
  write_distance start.key 0;
  while not (Set.is_empty !queue) do
    let u = Set.min_elt_exn !queue in
    queue := Set.remove !queue u;
    List.iter (neighbors_of u.key) ~f:(fun (key : key) ->
      if equal_point key.point u.key.point then failwith "same point";
      let new_distance = read_distance u.key + graph.(fst key.point).(snd key.point) in
      if new_distance < read_distance key
      then (
        queue := Set.add !queue { distance = new_distance; key };
        write_distance key new_distance))
  done;
  let end_point = Array.length graph - 1, Array.length graph.(0) - 1 in
  Map.keys !distance
  |> List.concat_map ~f:(fun key ->
    let point = point_from_visited_key key in
    if equal_point end_point point then [ Map.find_exn !distance key ] else [])
  |> Util.list_min
;;

let solve_1 input =
  let input2 =
    [ "2413432311323"
    ; "3215453535623"
    ; "3255245654254"
    ; "3446585845452"
    ; "4546657867536"
    ; "1438598798454"
    ; "4457876987766"
    ; "3637877979653"
    ; "4654967986887"
    ; "4564679986453"
    ; "1224686865563"
    ; "2546548887735"
    ; "4322674655533"
    ]
  in
  let city = parse input in
  dijkstras city
;;
