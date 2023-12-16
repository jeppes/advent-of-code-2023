open Core

type instruction =
  | Right
  | Left

let parse_instructions str =
  let parse_direction char =
    match char with
    | 'R' -> Some Right
    | 'L' -> Some Left
    | _ -> None
  in
  String.to_list str |> List.map ~f:parse_direction |> List.filter_map ~f:(fun x -> x)
;;

let parse_graph lines =
  let clean_string string = Str.global_replace (Str.regexp "[\\(|\\)|,|=]") "" string in
  let parse_graph_line line =
    let list = line |> clean_string |> String.split_on_chars ~on:[ ' ' ] in
    let name = List.nth_exn list 0 in
    let left = List.nth_exn list 2 in
    let right = List.nth_exn list 3 in
    name, (left, right)
  in
  List.map ~f:parse_graph_line lines |> String.Map.of_alist_exn
;;

let find name instruction graph =
  let destinations = Map.find_exn graph name in
  match instruction with
  | Left -> fst destinations
  | Right -> snd destinations
;;

let solve_1 input =
  let solve graph instructions =
    let rec traverse name count =
      let index = count mod List.length instructions in
      let instruction = List.nth_exn instructions index in
      let target = find name instruction graph in
      if String.( = ) target "ZZZ" then count + 1 else traverse target (count + 1)
    in
    traverse "AAA" 0
  in
  match input with
  | raw_instructions :: _ :: raw_graph ->
    let instructions = parse_instructions raw_instructions in
    let graph = parse_graph raw_graph in
    solve graph instructions
  | _ -> failwith "invalid input for day 8"
;;

let solve_2 input =
  let solve graph instructions =
    let starting_points =
      Map.to_alist graph
      |> List.map ~f:fst
      |> List.filter ~f:(String.is_suffix ~suffix:"A")
    in
    (* Number of steps that need to be taken before getting to a point that ends with Z *)
    let traverse name =
      let rec traverse_aux name count =
        let index = count mod List.length instructions in
        let instruction = List.nth_exn instructions index in
        let target = find name instruction graph in
        let is_finished = String.is_suffix ~suffix:"Z" name in
        if is_finished then count else traverse_aux target (count + 1)
      in
      traverse_aux name 0
    in
    List.map ~f:traverse starting_points |> Util.lcm_list
  in
  match input with
  | raw_instructions :: _ :: raw_graph ->
    let instructions = parse_instructions raw_instructions in
    let graph = parse_graph raw_graph in
    solve graph instructions
  | _ -> failwith "invalid input for day 8"
;;
