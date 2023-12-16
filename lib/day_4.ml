open Core

let parse lines =
  let parse_numbers string =
    String.split_on_chars ~on:[ ' ' ] string
    |> List.filter ~f:(fun x -> String.length x > 0)
    |> List.map ~f:int_of_string
  in
  let parse_line line =
    let parts = String.split_on_chars ~on:[ ':' ] line in
    let numbers_part = List.nth_exn parts 1 in
    let number_strings = String.split_on_chars ~on:[ '|' ] numbers_part in
    let left_numbers = List.nth_exn number_strings 0 |> parse_numbers in
    let right_numbers = List.nth_exn number_strings 1 |> parse_numbers in
    right_numbers |> List.filter ~f:(fun x -> List.mem left_numbers x ~equal:Int.equal)
  in
  List.map ~f:parse_line lines
;;

let solve_1 input =
  let value_of list =
    let exponent = float_of_int (List.length list - 1) in
    int_of_float (2.0 ** exponent)
  in
  parse input |> List.map ~f:value_of |> Util.sum
;;

let solve_2 input =
  let cards = parse input in
  let card_at_index index = List.nth_exn cards index in
  let matching_indices index =
    let card = card_at_index index in
    let number_of_matches = List.length card in
    List.range (index + 1) (index + number_of_matches + 1)
  in
  let rec solve index_queue index_to_count_map result =
    match index_queue with
    | [] -> result
    | index :: rest ->
      let matching_indices = matching_indices index in
      let result_for_index = Map.find_exn index_to_count_map index in
      let new_index_to_count_map =
        matching_indices
        |> List.map ~f:(fun x -> x, result_for_index)
        |> Int.Map.of_alist_exn
      in
      let merged_maps =
        Map.merge index_to_count_map new_index_to_count_map ~f:(fun ~key:_ ->
            function
            | `Left v -> Some v
            | `Right v -> Some v
            | `Both (v1, v2) -> Some (v1 + v2))
      in
      solve rest merged_maps (result + result_for_index)
  in
  let index_queue = List.range 0 (List.length cards) in
  let index_to_count_map =
    index_queue |> List.map ~f:(fun x -> x, 1) |> Int.Map.of_alist_exn
  in
  solve index_queue index_to_count_map 0
;;
