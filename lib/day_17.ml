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
[@@deriving eq, compare, sexp, hash]

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

type key =
  { point : int * int
  ; direction : direction
  ; steps : int
  }
[@@deriving compare, sexp, eq, hash]

type entry =
  { distance : int
  ; key : key
  }
[@@deriving compare, sexp, eq, hash]

module Key = struct
  type t = key [@@deriving compare, sexp, eq, hash]
end

module SetPoint = struct
  type t = entry [@@deriving sexp, hash]

  let compare a b =
    let distance_compare = compare a.distance b.distance in
    if distance_compare <> 0 then distance_compare else compare_entry a b
  ;;
end

module PointSet = Set.Make (SetPoint)

let find graph point = graph.(fst point).(snd point)

(*
   Original dijkstra's implementation in ocaml heavily inspired by:
   https://github.com/nilehmann/ocaml-algorithms/blob/master/dijkstra.ml
*)
let dijkstras graph ~neighbors_of =
  let result = ref Int.max_value in
  let distance = Hashtbl.create (module Key) in
  let write_distance key value = Hashtbl.set distance ~key ~data:value in
  let read_distance key : int =
    Hashtbl.find distance key |> Option.value ~default:Int.max_value
  in
  let queue = ref PointSet.empty in
  let enqueue entry = queue := Set.add !queue entry in
  let dequeue () =
    let min = Set.min_elt_exn !queue in
    queue := Set.remove !queue min;
    min
  in
  let starting_points =
    [ { distance = 0; key = { point = 0, 0; direction = Down; steps = 0 } }
    ; { distance = 0; key = { point = 0, 0; direction = Right; steps = 0 } }
    ]
  in
  List.iter starting_points ~f:(fun entry ->
    enqueue entry;
    write_distance entry.key 0);
  let end_point = Array.length graph - 1, Array.length graph.(0) - 1 in
  while not (Set.is_empty !queue) do
    let node = dequeue () in
    List.filter (neighbors_of node.key) ~f:(fun n -> is_valid_point graph n.point)
    |> List.iter ~f:(fun neighbor ->
      let new_distance = read_distance node.key + find graph neighbor.point in
      if new_distance < read_distance neighbor
      then (
        if equal_point end_point neighbor.point then result := min !result new_distance;
        queue := Set.add !queue { distance = new_distance; key = neighbor };
        write_distance neighbor new_distance))
  done;
  !result
;;

let turns direction =
  match direction with
  | Up -> [ Left; Right ]
  | Down -> [ Left; Right ]
  | Left -> [ Up; Down ]
  | Right -> [ Up; Down ]
;;

let neighbors_part_1 from_direction point steps =
  from_direction :: turns from_direction
  |> List.filter ~f:(fun direction ->
    if equal_direction direction from_direction && steps > 2 then false else true)
  |> List.map ~f:(fun direction ->
    let next_steps = if equal_direction direction from_direction then steps + 1 else 1 in
    let new_point = step_towards point direction in
    { point = new_point; direction; steps = next_steps })
;;

let neighbors_part_2 graph from_direction point steps =
  let end_point = Array.length graph - 1, Array.length graph.(0) - 1 in
  (match from_direction, steps with
   | direction, s when s >= 4 && s < 10 -> direction :: turns direction
   | direction, s when s <= 4 -> [ direction ]
   | direction, s when s >= 10 -> turns direction
   | _ -> failwith "invalid steps")
  |> List.map ~f:(fun direction ->
    let next_steps = if equal_direction direction from_direction then steps + 1 else 1 in
    let new_point = step_towards point direction in
    { point = new_point; direction; steps = next_steps })
  |> List.filter ~f:(fun n ->
    if equal_point n.point end_point then n.steps >= 4 else true)
;;

let solve_1 input =
  let graph = parse input in
  let neighbors_of n = neighbors_part_1 n.direction n.point n.steps in
  dijkstras graph ~neighbors_of
;;

let solve_2 input =
  let graph = parse input in
  let neighbors_of n = neighbors_part_2 graph n.direction n.point n.steps in
  dijkstras graph ~neighbors_of
;;
