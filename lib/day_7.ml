open Core

let create_weighted_list list =
  Char.Set.of_list list
  |> Set.to_list
  |> List.map ~f:(fun x -> x, List.count ~f:(fun y -> Char.( = ) x y) list)
  |> List.sort ~compare:(fun (_, a) (_, b) -> compare b a)
;;

let classify hand =
  let entries = create_weighted_list hand in
  let weights = List.map entries ~f:snd in
  match weights with
  | 5 :: _ -> "five-of-a-kind"
  | 4 :: _ -> "four-of-a-kind"
  | 3 :: 2 :: _ -> "full-house"
  | 3 :: _ -> "three-of-a-kind"
  | 2 :: 2 :: _ -> "two-pair"
  | 2 :: _ -> "one-pair"
  | _ -> "high-card"
;;

let hand_strength a =
  let hands =
    [ "high-card"
    ; "one-pair"
    ; "two-pair"
    ; "three-of-a-kind"
    ; "full-house"
    ; "four-of-a-kind"
    ; "five-of-a-kind"
    ]
  in
  Util.index_of_string hands a
;;

let solve_1 input =
  let card_strength (card : char) =
    let cards = [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ] in
    Util.index_of_char cards card
  in
  let rec compare_each hand_1 hand_2 =
    match hand_1, hand_2 with
    | a :: rest_1, b :: rest_2 ->
      if Char.( = ) a b
      then compare_each rest_1 rest_2
      else compare (card_strength a) (card_strength b)
    | _ -> failwith "Invalid hands. Hands must not be equal"
  in
  let compare_hands hand_1 hand_2 =
    let hand_1_class = classify hand_1 in
    let hand_2_class = classify hand_2 in
    if String.( <> ) hand_1_class hand_2_class
    then compare (hand_strength hand_1_class) (hand_strength hand_2_class)
    else compare_each hand_1 hand_2
  in
  let hands_sorted_by_strength =
    input
    |> List.map ~f:(fun line ->
      let parts = String.split_on_chars ~on:[ ' ' ] line in
      let hand = List.nth_exn parts 0 |> String.to_list in
      let bid = List.nth_exn parts 1 in
      hand, int_of_string bid)
    |> List.sort ~compare:(fun (hand_1, _) (hand_2, _) -> compare_hands hand_1 hand_2)
  in
  hands_sorted_by_strength |> List.mapi ~f:(fun i (_, bid) -> bid * (i + 1)) |> Util.sum
;;

let classify_joker hand =
  let weights = create_weighted_list hand in
  let weights_without_jokers =
    List.filter weights ~f:(fun (card, _) -> Char.( <> ) card 'J')
  in
  let largest_weight =
    if List.length weights_without_jokers = 0
    then 0
    else Util.list_max_by snd weights_without_jokers |> snd
  in
  let joker_weight =
    weights
    |> List.find ~f:(fun (card, _) -> Char.( = ) card 'J')
    |> Option.value ~default:('J', 0)
    |> snd
  in
  let pairs = List.filter weights_without_jokers ~f:(fun (_, weight) -> weight = 2) in
  match largest_weight, joker_weight, List.length pairs with
  | _, 0, _ -> classify hand
  | a, b, _ when a + b = 5 -> "five-of-a-kind"
  | a, b, _ when a + b = 4 -> "four-of-a-kind"
  | a, b, 2 when a + b = 3 -> "full-house"
  | a, b, _ when a + b = 3 -> "three-of-a-kind"
  | a, b, _ when a + b = 2 -> "one-pair"
  | _ -> "high-card"
;;

let solve_2 input =
  let card_strength (card : char) =
    let cards = [ 'J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A' ] in
    Util.index_of_char cards card
  in
  let rec compare_each hand_1 hand_2 =
    match hand_1, hand_2 with
    | a :: rest_1, b :: rest_2 ->
      if Char.( = ) a b
      then compare_each rest_1 rest_2
      else compare (card_strength a) (card_strength b)
    | _ -> failwith "Invalid hands. Hands must not be equal"
  in
  let compare_hands hand_1 hand_2 =
    let hand_1_class = classify_joker hand_1 in
    let hand_2_class = classify_joker hand_2 in
    if String.( <> ) hand_1_class hand_2_class
    then compare (hand_strength hand_1_class) (hand_strength hand_2_class)
    else compare_each hand_1 hand_2
  in
  let hands_sorted_by_strength =
    input
    |> List.map ~f:(fun line ->
      let parts = String.split_on_chars ~on:[ ' ' ] line in
      let hand = List.nth_exn parts 0 |> String.to_list in
      let bid = List.nth_exn parts 1 in
      hand, int_of_string bid)
    |> List.sort ~compare:(fun (hand_1, _) (hand_2, _) -> compare_hands hand_1 hand_2)
  in
  hands_sorted_by_strength |> List.mapi ~f:(fun i (_, bid) -> bid * (i + 1)) |> Util.sum
;;
