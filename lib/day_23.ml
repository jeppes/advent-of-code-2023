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

type graph = node array array

module Entry_Set = Set.Make (Entry)

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
  List.iteri input ~f:(fun row_index row ->
    String.iteri row ~f:(fun col_index char ->
      if row_index = 1 && col_index = 1 then print_endline (Char.to_string char);
      matrix.(row_index).(col_index) <- parse_char char));
  matrix
;;

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

let longest_path graph =
  let result = ref 0 in
  let starting_index, _ =
    graph.(0) |> Array.findi_exn ~f:(fun _ node -> equal_node node Empty)
  in
  let ending_index, _ =
    graph.(Array.length graph - 1)
    |> Array.findi_exn ~f:(fun _ node -> equal_node node Empty)
  in
  let start_point = 0, starting_index in
  let end_point = Array.length graph - 1, ending_index in
  let rec longest_path point visited count =
    if equal_point point end_point
    then (
      let previous_result = !result in
      result := max !result count;
      if previous_result <> !result
      then print_endline (show_point point ^ " " ^ Int.to_string !result);
      !result)
    else (
      let new_visited = Set.add visited point in
      let local_set = Hash_set.create (module Point) in
      Hash_set.add local_set point;
      let neighbors =
        ref
          (find_neighbors graph point
           |> List.filter ~f:(fun point -> not (Hash_set.mem local_set point))
           |> List.filter ~f:(fun point -> not (Set.mem new_visited point)))
      in
      let new_count = ref count in
      while
        List.length !neighbors = 1
        && not (List.mem !neighbors end_point ~equal:equal_point)
      do
        new_count := !new_count + 1;
        let neighbor = List.hd_exn !neighbors in
        Hash_set.add local_set neighbor;
        neighbors
        := find_neighbors graph neighbor
           |> List.filter ~f:(fun point -> not (Hash_set.mem local_set point))
           |> List.filter ~f:(fun point -> not (Set.mem new_visited point))
      done;
      let new_visited =
        Set.union new_visited (Hash_set.to_list local_set |> Point_set.of_list)
      in
      List.fold !neighbors ~init:0 ~f:(fun acc new_point ->
        let new_acc = longest_path new_point new_visited (!new_count + 1) in
        max acc new_acc))
  in
  longest_path start_point Point_set.empty 0
;;

let solve_1 input =
  let graph = parse input in
  longest_path graph
;;

(* Warning: extremely slow *)
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
  let result = longest_path graph in
  print_endline "done";
  result
;;
