open Core

let parse input = input |> List.map ~f:(String.split_on_chars ~on:[ ',' ]) |> List.concat

let hash string =
  string
  |> String.to_list
  |> List.fold_left ~init:0 ~f:(fun acc char ->
    let ascii = acc + Char.to_int char in
    let current = ascii * 17 in
    current mod 256)
;;

let solve_1 (input : string list) = parse input |> List.map ~f:hash |> Util.sum

type assignment =
  { box_index : int
  ; lens : string
  ; value : int
  }

type removal =
  { box_index : int
  ; lens : string
  }

type operation =
  | Assignment of assignment
  | Removal of removal

type box = (string * int) list [@@deriving eq]
type state = box option array

let run_operation (state : state) operation : state =
  let state = Array.copy state in
  let get_box box_index = state.(box_index) |> Option.value ~default:[] in
  match operation with
  | Assignment { box_index; lens; value } ->
    let box = get_box box_index in
    let contains_lens = List.exists box ~f:(fun (lens', _) -> String.( = ) lens' lens) in
    let new_box =
      if contains_lens
      then
        List.map box ~f:(fun (lens', value') ->
          lens', if String.equal lens' lens then value else value')
      else List.append box [ lens, value ]
    in
    state.(box_index) <- Some new_box;
    state
  | Removal { box_index; lens } ->
    let box = get_box box_index in
    let new_box = List.filter box ~f:(fun (lens', _) -> String.( <> ) lens' lens) in
    state.(box_index) <- Some new_box;
    state
;;

let parse_operation operation =
  let equal = String.contains operation '=' in
  if equal
  then (
    let parts = String.split operation ~on:'=' in
    let lens = List.hd_exn parts in
    let box_index = hash lens in
    let value = List.tl_exn parts |> List.hd_exn |> int_of_string in
    Assignment { box_index; lens; value })
  else (
    let parts = String.split operation ~on:'-' in
    let lens = List.hd_exn parts in
    let box_index = hash lens in
    Removal { box_index; lens })
;;

let lens_power box_index box =
  match box with
  | None -> 0
  | Some box ->
    List.mapi box ~f:(fun lens_index (_, value) ->
      (1 + box_index) * (1 + lens_index) * value)
    |> Util.sum
;;

let solve_2 input =
  parse input
  |> List.map ~f:parse_operation
  |> List.fold_left ~init:(Array.create ~len:256 None) ~f:run_operation
  |> Array.mapi ~f:lens_power
  |> Array.to_list
  |> Util.sum
;;
