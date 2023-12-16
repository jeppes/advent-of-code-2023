open Core

let parse_line string_line =
  String.split_on_chars ~on:[ ' ' ] string_line |> List.map ~f:int_of_string
;;

let rec differences list =
  match list with
  | a :: b :: rest -> (b - a) :: differences (b :: rest)
  | _ -> []
;;

let find_lists line =
  let rec find_lists_aux input acc =
    let result = differences input in
    if List.for_all ~f:(( = ) 0) input then acc else find_lists_aux result (result :: acc)
  in
  find_lists_aux line [ line ]
;;

let solve_1 string_lines =
  let solve_line line = find_lists line |> List.map ~f:List.last_exn |> Util.sum in
  string_lines |> List.map ~f:parse_line |> List.map ~f:solve_line |> Util.sum
;;

let solve_2 string_lines =
  let solve_line line =
    find_lists line
    |> List.map ~f:List.hd_exn
    |> List.fold_left ~f:(fun acc x -> x - acc) ~init:0
  in
  string_lines |> List.map ~f:parse_line |> List.map ~f:solve_line |> Util.sum
;;
