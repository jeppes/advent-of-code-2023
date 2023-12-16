open Core

let read_digit char =
  match char with
  | '1' -> Some 1
  | '2' -> Some 2
  | '3' -> Some 3
  | '4' -> Some 4
  | '5' -> Some 5
  | '6' -> Some 6
  | '7' -> Some 7
  | '8' -> Some 8
  | '9' -> Some 9
  | _ -> None
;;

let read_written_digits list =
  let rec read list acc =
    match list with
    | 'o' :: 'n' :: 'e' :: rest -> read ('e' :: rest) (1 :: acc)
    | 't' :: 'w' :: 'o' :: rest -> read ('o' :: rest) (2 :: acc)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest -> read ('e' :: rest) (3 :: acc)
    | 'f' :: 'o' :: 'u' :: 'r' :: rest -> read rest (4 :: acc)
    | 'f' :: 'i' :: 'v' :: 'e' :: rest -> read ('e' :: rest) (5 :: acc)
    | 's' :: 'i' :: 'x' :: rest -> read rest (6 :: acc)
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest -> read ('n' :: rest) (7 :: acc)
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest -> read ('t' :: rest) (8 :: acc)
    | 'n' :: 'i' :: 'n' :: 'e' :: rest -> read ('e' :: rest) (9 :: acc)
    | c :: rest ->
      (match read_digit c with
       | Some n -> read rest (n :: acc)
       | None -> read rest acc)
    | [] -> acc
  in
  read list [] |> List.rev
;;

let find_digit list = List.find_map list ~f:read_digit |> Option.value_exn

let find_numbers list =
  let first_digit = find_digit list in
  let second_digit = find_digit (List.rev list) in
  (first_digit * 10) + second_digit
;;

let solve_1 list = List.map list ~f:find_numbers |> Util.sum

let solve_2 list =
  let find_numbers list =
    let digits = read_written_digits list in
    let first_digit = List.hd_exn digits in
    let second_digit = List.hd_exn (List.rev digits) in
    (first_digit * 10) + second_digit
  in
  List.map list ~f:find_numbers |> Util.sum
;;
