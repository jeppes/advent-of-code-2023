open Core
open Point

type node =
  | Empty
  | Wall
  | Hill_Right
  | Hill_Left
  | Hill_Down
  | Hill_Up
[@@deriving eq, show]

type entry =
  { distance : int
  ; point : point
  ; previous : Point_set.t
  }
[@@deriving eq, compare, sexp]

module Entry = struct
  type t = entry [@@deriving eq, sexp]

  let compare a b =
    let distance_compare = compare a.distance b.distance in
    if distance_compare <> 0 then distance_compare else compare_entry a b
  ;;
end

type neighbor =
  { distance : int
  ; point : point
  }

type graph =
  { matrix : node array array
  ; neighbors_map : (point, neighbor list) Hashtbl.t
  }

module Entry_Set = Set.Make (Entry)

let find graph point = graph.(fst point).(snd point)

let find_neighbors graph point =
  let node = find graph point in
  let points =
    match node with
    | Empty -> neighbors_of_point_no_diagonals point
    | Wall -> []
    | Hill_Right -> [ fst point, snd point + 1 ]
    | Hill_Left -> [ fst point, snd point - 1 ]
    | Hill_Down -> [ fst point + 1, snd point ]
    | Hill_Up -> [ fst point - 1, snd point ]
  in
  List.filter points ~f:(fun (row, col) ->
    row >= 0
    && col >= 0
    && row < Array.length graph
    && col < Array.length graph.(0)
    && not (find graph (row, col) |> equal_node Wall))
;;

let parse input =
  let parse_char char =
    match char with
    | '.' -> Empty
    | '#' -> Wall
    | '>' -> Hill_Right
    | '<' -> Hill_Left
    | 'v' -> Hill_Down
    | '^' -> Hill_Up
    | _ -> failwith "Invalid char"
  in
  let row_count = List.length input in
  let col_count = String.length (List.hd_exn input) in
  let matrix = Array.make_matrix ~dimx:row_count ~dimy:col_count Empty in
  let neighbors_map = Hashtbl.create (module Point) in
  for i = 0 to row_count - 1 do
    for j = 0 to col_count - 1 do
      matrix.(i).(j) <- parse_char (String.get (List.nth_exn input i) j)
    done
  done;
  for i = 0 to row_count - 1 do
    for j = 0 to col_count - 1 do
      let node = matrix.(i).(j) in
      if not (equal_node Wall node)
      then (
        let neighbors =
          find_neighbors matrix (i, j)
          |> List.map ~f:(fun point -> { point; distance = 1 })
        in
        Hashtbl.set neighbors_map ~key:(i, j) ~data:neighbors)
    done
  done;
  let has_irrelevant_nodes () =
    List.filter (Hashtbl.keys neighbors_map) ~f:(fun point ->
      let neighbors = Hashtbl.find_exn neighbors_map point in
      List.length neighbors = 2)
    |> List.is_empty
    |> not
  in
  while has_irrelevant_nodes () do
    List.iter (Hashtbl.keys neighbors_map) ~f:(fun key ->
      let neighbors = Hashtbl.find_exn neighbors_map key in
      match neighbors with
      | [ p1; p2 ] ->
        let old_p1 = Hashtbl.find_exn neighbors_map p1.point in
        let new_p1 = old_p1 |> List.filter ~f:(fun p -> not (equal_point p.point key)) in
        let new_p2 =
          Hashtbl.find_exn neighbors_map p2.point
          |> List.filter ~f:(fun p -> not (equal_point p.point key))
        in
        Hashtbl.set
          neighbors_map
          ~key:p1.point
          ~data:({ p2 with distance = p1.distance + p2.distance } :: new_p1);
        Hashtbl.set
          neighbors_map
          ~key:p2.point
          ~data:({ p1 with distance = p2.distance + p1.distance } :: new_p2);
        Hashtbl.remove neighbors_map key
      | _ -> ())
  done;
  { matrix; neighbors_map }
;;

let longest_path (graph : graph) =
  let starting_index, _ =
    graph.matrix.(0) |> Array.findi_exn ~f:(fun _ node -> equal_node node Empty)
  in
  let ending_index, _ =
    graph.matrix.(Array.length graph.matrix - 1)
    |> Array.findi_exn ~f:(fun _ node -> equal_node node Empty)
  in
  let start_point = 0, starting_index in
  let end_point = Array.length graph.matrix - 1, ending_index in
  let rec longest_path point visited count =
    if equal_point point end_point
    then count
    else (
      let new_visited = Set.add visited point in
      let neighbors =
        Hashtbl.find_exn graph.neighbors_map point
        |> List.filter ~f:(fun n -> not (Set.mem visited n.point))
      in
      List.fold neighbors ~init:0 ~f:(fun acc neighbor ->
        let new_acc =
          longest_path neighbor.point new_visited (count + neighbor.distance)
        in
        max acc new_acc))
  in
  longest_path start_point Point_set.empty 0
;;

let solve_1 input =
  let graph = parse input in
  longest_path graph
;;

let solve_2 input =
  let input =
    input
    |> List.map ~f:(fun str ->
      String.map str ~f:(fun c ->
        match c with
        | '#' -> '#'
        | _ -> '.'))
  in
  let graph = parse input in
  longest_path graph
;;
