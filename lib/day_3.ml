open Core
open Point

type number_point =
  { number : int
  ; string_points : string list
  }

type symbol_point =
  { symbol : char
  ; point : point
  }

type position =
  | Number of number_point
  | Symbol of symbol_point

let parse input =
  let parse_line line row =
    let rec parse_position line column acc =
      let number_list = List.take_while ~f:Char.is_digit line in
      let number_list_length = List.length number_list in
      if number_list_length > 0
      then (
        let rest_after_number = List.drop_while ~f:Char.is_digit line in
        let new_column = column + number_list_length in
        let number = int_of_string (String.of_char_list number_list) in
        let points =
          List.range column new_column |> List.map ~f:(fun column -> row, column)
        in
        let string_points = points |> List.map ~f:show_point in
        let position = Number { string_points; number } in
        parse_position rest_after_number new_column (position :: acc))
      else (
        match line with
        | [] -> List.rev acc
        | c :: rest ->
          if Char.equal c '.'
          then parse_position rest (column + 1) acc
          else (
            let position = Symbol { point = row, column; symbol = c } in
            parse_position rest (column + 1) (position :: acc)))
    in
    let list = String.to_list line in
    parse_position list 0 []
  in
  List.range 0 (List.length input)
  |> List.map ~f:(fun row -> parse_line (List.nth_exn input row) row)
;;

let neighbors_of_symbol position =
  match position with
  | Symbol { point; _ } ->
    let neighbors = neighbors_of_point point |> List.map ~f:show_point in
    String.Set.of_list neighbors
  | _ -> String.Set.empty
;;

let get_numbers position =
  match position with
  | Number n -> [ n ]
  | _ -> []
;;

let get_gears position =
  match position with
  | Symbol { symbol = '*'; point } -> [ Symbol { symbol = '*'; point } ]
  | _ -> []
;;

let solve_1 input =
  let parsed = parse input in
  let neighbors_of_symbols =
    parsed |> List.concat |> List.map ~f:neighbors_of_symbol |> String.Set.union_list
  in
  let numbers = parsed |> List.concat |> List.concat_map ~f:get_numbers in
  let numbers_neighboring_symbols =
    List.filter
      ~f:(fun p ->
        List.exists ~f:(fun x -> Set.mem neighbors_of_symbols x) p.string_points)
      numbers
  in
  numbers_neighboring_symbols |> List.map ~f:(fun x -> x.number) |> Util.sum
;;

let solve_2 input =
  let parsed = parse input in
  let gears = parsed |> List.concat |> List.concat_map ~f:get_gears in
  let numbers = parsed |> List.concat |> List.concat_map ~f:get_numbers in
  let number_matches_gear gear_neighbors number =
    List.exists ~f:(fun point -> Set.mem gear_neighbors point) number.string_points
  in
  let gear_product gear =
    let neighbors = neighbors_of_symbol gear in
    let matching_numbers = numbers |> List.filter ~f:(number_matches_gear neighbors) in
    if List.length matching_numbers = 2
    then matching_numbers |> List.map ~f:(fun x -> x.number) |> Util.product
    else 0
  in
  gears |> List.map ~f:gear_product |> Util.sum
;;
