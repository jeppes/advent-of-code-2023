open Core

type destination =
  | Workflow of string
  | Accept
  | Reject
[@@deriving show, eq]

type operation =
  | Less_Than of (string * int * destination)
  | Greater_Than of (string * int * destination)
[@@deriving show, eq]

type part = int String.Map.t

let show_part part =
  Map.to_alist part
  |> List.map ~f:(fun (k, v) -> k ^ "=" ^ Int.to_string v)
  |> String.concat ~sep:","
  |> fun s -> "{" ^ s ^ "}"
;;

type workflow =
  { name : string
  ; operations : operation list
  ; destination : destination
  }
[@@deriving show, eq]

let parse input =
  let workflows_strings = List.take_while input ~f:(fun s -> String.( <> ) s "") in
  let part_strings =
    List.drop (List.drop_while input ~f:(fun s -> String.( <> ) s "")) 1
  in
  let parse_destination string =
    match string with
    | "A" -> Accept
    | "R" -> Reject
    | s -> Workflow s
  in
  let parse_operation string =
    let split = String.split_on_chars string ~on:[ '<'; '>'; ':' ] in
    match split with
    | [ part; number; next ] ->
      let is_less_than = String.contains string '<' in
      if is_less_than
      then Less_Than (part, Int.of_string number, parse_destination next)
      else Greater_Than (part, Int.of_string number, parse_destination next)
    | _ -> failwith ("invalid operation string: " ^ string)
  in
  let parse_workflow line =
    let parts = String.split_on_chars line ~on:[ '{'; '}'; ',' ] |> List.drop_last_exn in
    let name = List.nth_exn parts 0 in
    let operations =
      parts |> List.tl_exn |> List.drop_last_exn |> List.map ~f:parse_operation
    in
    let destination = List.nth_exn parts (List.length parts - 1) |> parse_destination in
    name, { name; operations; destination }
  in
  let workflows =
    List.map workflows_strings ~f:parse_workflow |> String.Map.of_alist_exn
  in
  let parse_parts line =
    let components =
      String.split_on_chars line ~on:[ '{'; '}'; ',' ] |> List.drop_last_exn
    in
    components
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:(fun s ->
      let ps = String.split_on_chars s ~on:[ '=' ] in
      List.nth_exn ps 0, Int.of_string (List.nth_exn ps 1))
    |> String.Map.of_alist_exn
  in
  let parts = List.map part_strings ~f:parse_parts in
  workflows, parts
;;

type operation_result =
  | Destination of destination
  | Continue
[@@deriving show]

let find_acceptable_parts workflows parts =
  let apply operation part =
    match operation with
    | Less_Than (part_name, number, destination) ->
      let part_value = Map.find_exn part part_name in
      if part_value < number then Destination destination else Continue
    | Greater_Than (part_name, number, destination) ->
      let part_value = Map.find_exn part part_name in
      if part_value > number then Destination destination else Continue
  in
  let rec run_workflow workflow part =
    let result =
      List.fold_left workflow.operations ~init:Continue ~f:(fun result operation ->
        match result with
        | Continue -> apply operation part
        | _ -> result)
    in
    match result with
    | Continue ->
      (match workflow.destination with
       | Workflow workflow ->
         let next_workflow = Map.find_exn workflows workflow in
         run_workflow next_workflow part
       | _ -> workflow.destination)
    | Destination destination ->
      (match destination with
       | Workflow workflow ->
         let next_workflow = Map.find_exn workflows workflow in
         run_workflow next_workflow part
       | _ -> destination)
  in
  let sum_part part = Map.to_alist part |> List.map ~f:snd |> Util.sum in
  let start = Map.find_exn workflows "in" in
  List.map parts ~f:(fun part ->
    let result = run_workflow start part in
    match result with
    | Workflow _ -> failwith "invalid result, cannot end in a workflow"
    | Accept -> sum_part part
    | Reject -> 0)
  |> Util.sum
;;

let solve_1 input =
  let workflows, parts = parse input in
  let result = find_acceptable_parts workflows parts in
  result
;;

let operation_destination operation =
  match operation with
  | Less_Than (_, _, destination) -> destination
  | Greater_Than (_, _, destination) -> destination
;;

type range =
  | Lt of (string * int)
  | Gt of (string * int)
  | Lte of (string * int)
  | Gte of (string * int)
[@@deriving show, eq]

let operation_to_range operation =
  match operation with
  | Less_Than (part, number, _) -> Lt (part, number)
  | Greater_Than (part, number, _) -> Gt (part, number)
;;

let invert_range range =
  match range with
  | Lt (part, number) -> Gte (part, number)
  | Gt (part, number) -> Lte (part, number)
  | Lte (part, number) -> Gt (part, number)
  | Gte (part, number) -> Lt (part, number)
;;

let range_part range =
  match range with
  | Lt (part, _) -> part
  | Gt (part, _) -> part
  | Lte (part, _) -> part
  | Gte (part, _) -> part
;;

let find_values_in_ranges ranges =
  List.map [ "x"; "m"; "a"; "s" ] ~f:(fun target ->
    let lower, upper =
      List.filter ranges ~f:(fun range -> String.( = ) (range_part range) target)
      |> List.fold_left ~init:(1, 4000) ~f:(fun (lower, upper) range ->
        match range with
        | Lt (_, number) -> lower, min upper (number - 1)
        | Gt (_, number) -> max lower (number + 1), upper
        | Lte (_, number) -> lower, min upper number
        | Gte (_, number) -> max lower number, upper)
    in
    target, lower, upper)
;;

let find_all_possible_combinations workflows =
  let rec evaluate_operation operation (ranges : range list) : range list list =
    match operation_destination operation with
    | Workflow workflow_name ->
      let workflow = Map.find_exn workflows workflow_name in
      evaluate_workflow workflow (operation_to_range operation :: ranges)
    | Accept -> [ operation_to_range operation :: ranges ]
    | Reject -> []
  and evaluate_workflow workflow (ranges : range list) : range list list =
    let ranges_inner, inverted_ranges =
      List.fold_left
        workflow.operations
        ~init:([], [])
        ~f:(fun (inner_ranges, inverted_ranges) operation ->
          let new_inner = evaluate_operation operation (inverted_ranges @ ranges) in
          let inverted_range = invert_range (operation_to_range operation) in
          new_inner @ inner_ranges, inverted_range :: inverted_ranges)
    in
    let ranges_after =
      match workflow.destination with
      | Workflow workflow_name ->
        let workflow = Map.find_exn workflows workflow_name in
        evaluate_workflow workflow (ranges @ inverted_ranges)
      | Accept -> [ inverted_ranges @ ranges ]
      | Reject -> []
    in
    ranges_inner @ ranges_after
  in
  let start = Map.find_exn workflows "in" in
  evaluate_workflow start []
;;

let solve_2 input =
  let workflows, _ = parse input in
  let combinations = find_all_possible_combinations workflows in
  List.map combinations ~f:(fun ranges ->
    find_values_in_ranges ranges
    |> List.fold_left ~init:1 ~f:(fun acc (_, lower, upper) -> acc * (upper - lower + 1)))
  |> Util.sum
;;
