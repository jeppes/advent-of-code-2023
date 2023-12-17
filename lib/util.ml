open Core

let read_file name = In_channel.read_lines name ~fix_win_eol:true
let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / gcd m n
;;

let lcm_list = List.fold_left ~init:1 ~f:lcm
let sum list = List.fold_left ~f:( + ) ~init:0 list
let product list = List.fold_left ~init:1 ~f:(fun acc x -> acc * x) list

let format_ns ns =
  if Float.( <. ) ns 1000000.0
  then string_of_int (int_of_float (ns /. 1000.0)) ^ "μs"
  else string_of_int (int_of_float (ns /. 1000000.0)) ^ "ms"
;;

type 'a measurement =
  { value : 'a
  ; time_ns : float
  }

let measure_ns f x =
  let start = Core.Time_ns.now () in
  let value = f x in
  let stop = Core.Time_ns.now () in
  let time_ns = Time_ns.diff stop start |> Time_ns.Span.to_ns in
  { value; time_ns }
;;

let make_measurement ~value ~time_ns = { value; time_ns }

let to_result ~result ~expected =
  let time_string = "[" ^ format_ns result.time_ns ^ "]" in
  if result.value = expected
  then "⭐ " ^ time_string
  else "❌, expected " ^ string_of_int expected ^ ", got " ^ string_of_int result.value
;;

let list_last list = List.nth_exn list (List.length list - 1)

let cut_off prefix string =
  if String.is_prefix ~prefix string
  then (
    let prefix_length = String.length prefix in
    String.sub string ~pos:prefix_length ~len:(String.length string - prefix_length))
  else failwith "Expected string \"" ^ string ^ "\" to start with \"" ^ prefix ^ "\""
;;

let list_min list =
  List.min_elt ~compare:(fun a b -> compare a b) list |> Option.value_exn
;;

let list_max list =
  List.max_elt ~compare:(fun a b -> compare a b) list |> Option.value_exn
;;

let list_max_by fn list =
  List.max_elt ~compare:(fun a b -> compare (fn a) (fn b)) list |> Option.value_exn
;;

let is_even n = n mod 2 = 0
let is_odd n = not (is_even n)

let index_of_char list value =
  List.findi list ~f:(fun _ x -> Char.( = ) x value) |> Option.value_exn |> fst
;;

let index_of_string list value =
  List.findi list ~f:(fun _ x -> String.( = ) x value) |> Option.value_exn |> fst
;;

let list_to_pairs list =
  let rec list_to_pairs_aux list acc =
    match list with
    | a :: b :: rest -> list_to_pairs_aux rest ((a, b) :: acc)
    | [] -> acc
    | _ -> failwith "invalid pair list"
  in
  list_to_pairs_aux list [] |> List.rev
;;

let manhattan_distance (x_1, y_1) (x_2, y_2) = abs (x_1 - x_2) + abs (y_1 - y_2)
