open Core
open Point

type connected_tile =
  { id : string
  ; symbol : char
  ; at : point
  ; connects_to : point list
  }

type start_tile =
  { id : string
  ; symbol : char
  ; at : point
  }

type empty_tile =
  { id : string
  ; symbol : char
  ; at : point
  }

type tile =
  | Start of start_tile
  | Empty of empty_tile
  | Connected of connected_tile

let parse_tile symbol at =
  let row = fst at in
  let col = snd at in
  let id = show_point at in
  match symbol with
  | '|' -> Connected { id; symbol; at; connects_to = [ row - 1, col; row + 1, col ] }
  | '-' -> Connected { id; symbol; at; connects_to = [ row, col - 1; row, col + 1 ] }
  | 'L' -> Connected { id; symbol; at; connects_to = [ row - 1, col; row, col + 1 ] }
  | 'J' -> Connected { id; symbol; at; connects_to = [ row - 1, col; row, col - 1 ] }
  | '7' -> Connected { id; symbol; at; connects_to = [ row + 1, col; row, col - 1 ] }
  | 'F' -> Connected { id; symbol; at; connects_to = [ row + 1, col; row, col + 1 ] }
  | '.' -> Empty { id; symbol; at }
  | 'S' -> Start { id; symbol; at }
  | c -> failwith ("invalid symbol in graph: " ^ String.make 1 c)
;;

let parse lines =
  let parse_row line row =
    let array = String.to_array line in
    Array.mapi ~f:(fun col char -> parse_tile char (row, col)) array
  in
  Array.mapi ~f:(fun row line -> parse_row line row) lines
;;

let get_start graph =
  graph
  |> Array.find_map ~f:(fun row ->
    Array.find_map
      ~f:(fun tile ->
        match tile with
        | Start _ -> Some tile
        | _ -> None)
      row)
  |> Option.value_exn
;;

let tile_symbol tile =
  match tile with
  | Start n -> n.symbol
  | Connected n -> n.symbol
  | Empty n -> n.symbol
;;

let tile_at graph point =
  let number_of_rows = Array.length graph in
  let number_of_columns = Array.length (Array.get graph 0) in
  let row_out_of_bounds = fst point < 0 || fst point >= number_of_rows in
  let col_out_of_bounds = snd point < 0 || snd point >= number_of_columns in
  if row_out_of_bounds || col_out_of_bounds
  then None
  else (
    let row = Array.get graph (fst point) in
    Some (Array.get row (snd point)))
;;

let tile_location tile =
  match tile with
  | Start n -> n.at
  | Connected n -> n.at
  | Empty n -> n.at
;;

let tile_id tile =
  match tile with
  | Start n -> n.id
  | Connected n -> n.id
  | Empty n -> n.id
;;

let equal_tile tile_1 tile_2 =
  let id_1 = tile_id tile_1 in
  let id_2 = tile_id tile_2 in
  String.( = ) id_1 id_2
;;

let find_loop graph =
  let neighbors_of tile =
    match tile with
    | Start n -> neighbors_of_point n.at |> List.filter_map ~f:(tile_at graph)
    | Connected n -> n.connects_to |> List.filter_map ~f:(tile_at graph)
    | Empty _ -> []
  in
  let rec find_cycle tile previous visited =
    match tile with
    | Start _ -> [ visited ]
    | Empty _ -> [ String.Set.empty ]
    | Connected _ ->
      let neighbors =
        neighbors_of tile
        |> List.filter ~f:(fun new_tile -> not (equal_tile previous new_tile))
      in
      let tile_key = tile_id tile in
      neighbors
      |> List.concat_map ~f:(fun new_tile ->
        find_cycle new_tile tile (Set.add visited tile_key))
  in
  let start = get_start graph in
  let visited = String.Set.singleton (tile_id start) in
  let paths : String.Set.t list =
    neighbors_of start
    |> List.concat_map ~f:(fun new_tile -> find_cycle new_tile start visited)
  in
  let loops : String.Set.t list =
    paths |> List.filter ~f:(fun x -> not (Set.is_empty x))
  in
  loops |> Util.list_max_by Set.length
;;

let solve_2 graph loop =
  let is_in_loop tile = Set.mem loop (tile_id tile) in
  let all_tiles = graph |> Array.to_list |> Array.concat |> Array.to_list in
  (* Shoot a ray from the current tile outwards and count the the number
     of intersections with the loop. If this number is odd, the tile is
     inside the loop *)
  let is_inside_loop tile =
    let row, col = tile_location tile in
    let rec find_ray_path (row, col) acc =
      let tile = tile_at graph (row, col) in
      match tile with
      | None -> acc
      | Some tile -> find_ray_path (row + 1, col + 1) (tile :: acc)
    in
    let is_intersection tile =
      let symbol = tile_symbol tile in
      (* NOTE: this assumes 'S' is '7' or 'L', which is true in my input but not generally *)
      is_in_loop tile
      && Char.( <> ) symbol '7'
      && Char.( <> ) symbol 'L'
      && Char.( <> ) symbol 'S'
    in
    let ray_path = find_ray_path (row, col) [] in
    let number_of_intersections =
      ray_path |> List.filter ~f:is_intersection |> List.length
    in
    Util.is_odd number_of_intersections
  in
  let tiles_inside_loop _ =
    all_tiles
    |> List.filter ~f:(fun tile -> not (is_in_loop tile))
    |> List.filter ~f:is_inside_loop
  in
  let tiles_in_loop = tiles_inside_loop () in
  List.length tiles_in_loop
;;

let solve_1 loop = Set.length loop / 2
