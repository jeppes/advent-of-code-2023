open Core

type hand =
  { color : string
  ; count : int
  }

type game =
  { id : int
  ; rounds : hand list list
  }

let parse lines =
  let parse_entry entry =
    let parts =
      String.split_on_chars ~on:[ ' ' ] entry
      |> List.filter ~f:(fun x -> String.length x > 0)
    in
    let count = List.nth_exn parts 0 |> int_of_string in
    let color = List.nth_exn parts 1 in
    { count; color }
  in
  let parse_rounds game =
    let entries = String.split_on_chars ~on:[ ',' ] game in
    List.map ~f:parse_entry entries
  in
  let parse_game line =
    let parts = String.split_on_chars ~on:[ ':' ] line in
    let id_part = List.nth_exn parts 0 in
    let id = Util.cut_off "Game " id_part |> int_of_string in
    let game_part = List.nth_exn parts 1 in
    let games = String.split_on_chars ~on:[ ';' ] game_part in
    let rounds = List.map ~f:parse_rounds games in
    { id; rounds }
  in
  List.map ~f:parse_game lines
;;

let count_color color round =
  let hand = List.find ~f:(fun x -> String.( = ) x.color color) round in
  match hand with
  | None -> 0
  | Some hand -> hand.count
;;

let is_round_possible round =
  let red_count = count_color "red" round in
  let green_count = count_color "green" round in
  let blue_count = count_color "blue" round in
  red_count <= 12 && green_count <= 13 && blue_count <= 14
;;

let is_game_possible game = List.for_all ~f:is_round_possible game.rounds

let solve_1 input =
  let games = parse input in
  List.filter ~f:is_game_possible games |> List.map ~f:(fun x -> x.id) |> Util.sum
;;

let solve_2 input =
  let count_colors color game =
    game.rounds |> List.map ~f:(count_color color) |> Util.list_max
  in
  let power_set game =
    let red_counts = count_colors "red" game in
    let green_counts = count_colors "green" game in
    let blue_count = count_colors "blue" game in
    red_counts * green_counts * blue_count
  in
  let games = parse input in
  List.map ~f:power_set games |> Util.sum
;;
