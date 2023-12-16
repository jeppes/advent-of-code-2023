open Core

let transpose lines =
  let cols = String.length (List.hd_exn lines) in
  List.range 0 cols
  |> List.map ~f:(fun col ->
    let new_line =
      List.map (List.rev lines) ~f:(fun row -> String.get row col |> String.of_char)
    in
    String.concat new_line)
;;

let rec split_into_cases input current acc =
  match input with
  | [] -> List.rev (List.rev current :: acc)
  | "" :: rest -> split_into_cases rest [] (List.rev current :: acc)
  | line :: rest -> split_into_cases rest (line :: current) acc
;;

let solve_1 input =
  let find_mirror all_lines =
    let rec find_mirrors_aux lines index =
      match lines with
      | a :: b :: rest when String.( = ) a b ->
        let list_before = List.take all_lines (index + 1) |> List.rev in
        let list_after = List.drop all_lines (index + 1) in
        let zipped, _ = List.zip_with_remainder list_before list_after in
        let is_mirror = List.for_all zipped ~f:(fun (a, b) -> String.( = ) a b) in
        if is_mirror then index + 1 else find_mirrors_aux (b :: rest) (index + 1)
      | _ :: rest -> find_mirrors_aux rest (index + 1)
      | [] -> 0
    in
    find_mirrors_aux all_lines 0
  in
  let solve case =
    let horizontal = find_mirror case in
    let vertical = find_mirror (transpose case) in
    if horizontal > vertical then horizontal * 100 else vertical
  in
  split_into_cases input [] [] |> List.map ~f:solve |> Util.sum
;;

let solve_2 input =
  let smudged_equal a b =
    let rec smudged_equal_aux a b smudges =
      match a, b, smudges with
      | _, _, s when s > 1 -> false
      | c1 :: rest1, c2 :: rest2, s when Char.( <> ) c1 c2 ->
        smudged_equal_aux rest1 rest2 (s + 1)
      | c1 :: rest1, c2 :: rest2, s when Char.( = ) c1 c2 ->
        smudged_equal_aux rest1 rest2 s
      | [], [], c -> c = 1
      | _ -> failwith "Expected strings of equal length"
    in
    smudged_equal_aux (String.to_list a) (String.to_list b) 0
  in
  let find_mirror_with_smudge all_lines =
    let rec find_mirrors_aux lines index =
      match lines with
      | _ :: rest ->
        let list_before = List.take all_lines (index + 1) |> List.rev in
        let list_after = List.drop all_lines (index + 1) in
        let zipped, _ = List.zip_with_remainder list_before list_after in
        let possible_smudge_indices = List.range 0 (List.length zipped) in
        let has_smudge =
          List.exists possible_smudge_indices ~f:(fun smudge_index ->
            List.for_alli zipped ~f:(fun i (a, b) ->
              if i = smudge_index then smudged_equal a b else String.( = ) a b))
        in
        if has_smudge then index + 1 else find_mirrors_aux rest (index + 1)
      | [] -> 0
    in
    find_mirrors_aux all_lines 0
  in
  let solve case =
    let horizontal = find_mirror_with_smudge case in
    let vertical = find_mirror_with_smudge (transpose case) in
    if horizontal > vertical then horizontal * 100 else vertical
  in
  split_into_cases input [] [] |> List.map ~f:solve |> Util.sum
;;
