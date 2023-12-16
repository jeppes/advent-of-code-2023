open Core

type node =
  | Empty
  | RoundRock
  | CubeRock
[@@deriving eq]

let to_char node =
  match node with
  | Empty -> '.'
  | RoundRock -> 'O'
  | CubeRock -> '#'
;;

let of_char char =
  match char with
  | '.' -> Empty
  | 'O' -> RoundRock
  | '#' -> CubeRock
  | _ -> failwith ("invalid node " ^ String.of_char char)
;;

let graph_to_string (graph : node array array) =
  Array.map graph ~f:(fun row ->
    row
    |> Array.to_list
    |> List.map ~f:to_char
    |> List.map ~f:String.of_char
    |> String.concat)
  |> Array.to_list
  |> String.concat ~sep:"\n"
;;

let parse input =
  let parse_line line = line |> String.to_list |> List.map ~f:of_char |> List.to_array in
  List.map input ~f:parse_line |> List.to_array
;;

type direction =
  | North
  | East
  | South
  | West
[@@deriving eq]

let move_rocks directions graph =
  let copy_graph graph = Array.map (Array.copy graph) ~f:Array.copy in
  let graph = copy_graph graph in
  let row_count = Array.length graph in
  let col_count = Array.length graph.(0) in
  let move_rocks_north graph =
    for row = 0 to row_count - 1 do
      for col = 0 to col_count - 1 do
        let node = graph.(row).(col) in
        if equal_node RoundRock node
        then (
          let finished = ref false in
          let step = -1 in
          let new_row = ref (row + step) in
          while (not !finished) && !new_row >= 0 && !new_row < row_count do
            let other_node = graph.(!new_row).(col) in
            if equal_node other_node Empty
            then (
              graph.(!new_row).(col) <- node;
              graph.(!new_row - step).(col) <- Empty)
            else finished := true;
            new_row := !new_row + step
          done)
      done
    done;
    graph
  in
  let move_rocks_south graph =
    for row = row_count - 1 downto 0 do
      for col = 0 to col_count - 1 do
        let node = graph.(row).(col) in
        if equal_node RoundRock node
        then (
          let finished = ref false in
          let step = 1 in
          let new_row = ref (row + step) in
          while (not !finished) && !new_row >= 0 && !new_row < row_count do
            let other_node = graph.(!new_row).(col) in
            if equal_node other_node Empty
            then (
              graph.(!new_row).(col) <- node;
              graph.(!new_row - step).(col) <- Empty)
            else finished := true;
            new_row := !new_row + step
          done)
      done
    done;
    graph
  in
  let move_rocks_west graph =
    for row = 0 to row_count - 1 do
      for col = 0 to col_count - 1 do
        let node = graph.(row).(col) in
        if equal_node RoundRock node
        then (
          let finished = ref false in
          let step = -1 in
          let new_col = ref (col + step) in
          while (not !finished) && !new_col >= 0 && !new_col < col_count do
            let other_node = graph.(row).(!new_col) in
            if equal_node other_node Empty
            then (
              graph.(row).(!new_col) <- node;
              graph.(row).(!new_col - step) <- Empty)
            else finished := true;
            new_col := !new_col + step
          done)
      done
    done;
    graph
  in
  let move_rocks_east graph =
    for row = 0 to row_count - 1 do
      for col = col_count - 1 downto 0 do
        let node = graph.(row).(col) in
        if equal_node RoundRock node
        then (
          let finished = ref false in
          let step = 1 in
          let new_col = ref (col + step) in
          while (not !finished) && !new_col >= 0 && !new_col < col_count do
            let other_node = graph.(row).(!new_col) in
            if equal_node other_node Empty
            then (
              graph.(row).(!new_col) <- node;
              graph.(row).(!new_col - step) <- Empty)
            else finished := true;
            new_col := !new_col + step
          done)
      done
    done;
    graph
  in
  List.iter directions ~f:(fun direction ->
    match direction with
    | North -> move_rocks_north graph |> ignore
    | South -> move_rocks_south graph |> ignore
    | East -> move_rocks_east graph |> ignore
    | West -> move_rocks_west graph |> ignore);
  graph
;;

let score graph =
  let multiplier i = Array.length graph - i in
  Array.foldi graph ~init:0 ~f:(fun i acc row ->
    acc + (Array.count row ~f:(fun node -> equal_node node RoundRock) * multiplier i))
;;

let solve_1 input =
  let graph = move_rocks [ North ] (parse input) in
  score graph
;;

let cycle graph = move_rocks [ North; West; South; East ] graph

let find_loop graph =
  let graph_list = ref [] in
  let graph = ref (cycle graph) in
  let graph_string = ref (graph_to_string !graph) in
  let is_loop_closed _ = List.mem !graph_list !graph_string ~equal:String.( = ) in
  while not (is_loop_closed ()) do
    graph_list := !graph_string :: !graph_list;
    graph := cycle !graph;
    graph_string := graph_to_string !graph
  done;
  graph_list := List.rev !graph_list;
  let loop_start_index, _ =
    List.findi_exn !graph_list ~f:(fun _ s -> String.( = ) !graph_string s)
  in
  let loop = List.drop !graph_list loop_start_index in
  let loop_offset = loop_start_index + 1 in
  loop, loop_offset
;;

let solve_2 input =
  let graph = parse input in
  let loop, loop_offset = find_loop graph in
  let cycles = 1000000000 - loop_offset in
  let billionth_graph =
    cycles mod List.length loop |> List.nth_exn loop |> String.split_lines |> parse
  in
  score billionth_graph
;;
