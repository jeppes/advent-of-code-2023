open Core

let matches_template template (group_index, group) previous_index ~check_end =
  let is_legal_spring char = Char.( = ) char '?' || Char.( = ) char '#' in
  let is_legal_space char = Char.( = ) char '?' || Char.( = ) char '.' in
  if group_index >= Array.length template then failwith "invalid group index";
  let matches = ref true in
  for index = previous_index to group_index + group do
    matches
    := !matches
       && (index >= Array.length template
           ||
           let template_char = template.(index) in
           if index < group_index || index >= group_index + group
           then is_legal_space template_char
           else is_legal_spring template_char)
  done;
  if check_end
  then
    for index = group_index + group to Array.length template - 1 do
      if not (is_legal_space template.(index)) then matches := false
    done;
  !matches
;;

let solve template groups =
  let groups_length = Array.length groups in
  let template_length = String.length template in
  let template_array = template |> String.to_array in
  let cache =
    List.range 0 groups_length
    |> List.map ~f:(fun _ ->
      List.range 0 template_length |> List.map ~f:(fun _ -> -1) |> Array.of_list)
    |> Array.of_list
  in
  let rec possible_positions starting_index group_index =
    if group_index = groups_length
    then 1
    else if cache.(group_index).(starting_index) <> -1
    then cache.(group_index).(starting_index)
    else (
      let group = groups.(group_index) in
      let groups_after =
        Array.foldi
          groups
          ~f:(fun i acc x -> if i > group_index then acc + x + 1 else 0)
          ~init:0
      in
      let first_possible_index = starting_index in
      let last_possible_index = template_length - group - groups_after in
      let result =
        List.range first_possible_index (last_possible_index + 1)
        |> List.map ~f:(fun index ->
          if matches_template
               template_array
               (index, group)
               starting_index
               ~check_end:(group_index = groups_length - 1)
          then possible_positions (index + group + 1) (group_index + 1)
          else 0)
        |> Util.sum
      in
      cache.(group_index).(starting_index) <- result;
      result)
  in
  possible_positions 0 0
;;

let solve_1 input =
  List.map input ~f:(fun line ->
    let split = String.split_on_chars ~on:[ ' ' ] line in
    let template = List.nth_exn split 0 in
    let groups = List.nth_exn split 1 |> String.split_on_chars ~on:[ ',' ] in
    let groups = List.map ~f:int_of_string groups |> Array.of_list in
    solve template groups)
  |> Util.sum
;;

let solve_2 input =
  let copy string ~times ~sep =
    List.range 0 times |> List.map ~f:(fun _ -> string) |> String.concat ~sep
  in
  List.map input ~f:(fun line ->
    let split = String.split_on_chars ~on:[ ' ' ] line in
    let _template = List.nth_exn split 0 in
    let template = copy _template ~times:5 ~sep:"?" in
    let _groups = List.nth_exn split 1 in
    let groups = copy _groups ~times:5 ~sep:"," |> String.split_on_chars ~on:[ ',' ] in
    let groups = List.map ~f:int_of_string groups |> Array.of_list in
    solve template groups)
  |> Util.sum
;;
