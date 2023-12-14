open Core
open Aoc2023

module Day_1 = struct
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
end

module Day_2 = struct
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
end

type point = int * int [@@deriving show]

let neighbors_of_point point =
  let row = fst point in
  let column = snd point in
  [ (*  Row above *)
    row - 1, column - 1
  ; row - 1, column
  ; row - 1, column + 1
  ; (* Current row *)
    row, column - 1
  ; row, column + 1
  ; (* Row below *)
    row + 1, column - 1
  ; row + 1, column
  ; row + 1, column + 1
  ]
;;

module Day_3 = struct
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
end

module Day_4 = struct
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
end

module Day_5 = struct
  type range =
    { low : int
    ; high : int
    ; transform : int
    }

  type map = { ranges : range list }

  type problem =
    { seeds : int list
    ; maps : map list
    }

  let parse input =
    let parse_seeds line =
      String.sub line ~pos:7 ~len:(String.length line - 7)
      |> String.split_on_chars ~on:[ ' ' ]
      |> List.map ~f:int_of_string
    in
    let create_range ~source ~destination ~size =
      let transform = destination - source in
      let point_1 = source in
      let point_2 = source + (size - 1) in
      { low = min point_1 point_2; high = max point_1 point_2; transform }
    in
    let rec parse_maps lines current_map maps =
      match lines with
      | [] ->
        let new_maps =
          match current_map with
          | Some { ranges } -> { ranges } :: maps
          | None -> maps
        in
        List.rev new_maps
      | line :: new_lines ->
        let is_start_of_map = String.is_suffix line ~suffix:" map:" in
        if is_start_of_map
        then (
          let new_maps =
            match current_map with
            | Some { ranges } -> { ranges } :: maps
            | None -> maps
          in
          let new_current_map = { ranges = [] } in
          parse_maps new_lines (Some new_current_map) new_maps)
        else (
          let split = String.split_on_chars ~on:[ ' ' ] line in
          let destination = List.nth_exn split 0 |> int_of_string in
          let source = List.nth_exn split 1 |> int_of_string in
          let size = List.nth_exn split 2 |> int_of_string in
          let new_range = create_range ~destination ~source ~size in
          let map = Option.value_exn current_map in
          let ranges = new_range :: map.ranges in
          parse_maps new_lines (Some { ranges }) maps)
    in
    let sort_map { ranges } =
      { ranges = List.sort ranges ~compare:(fun a b -> compare a.low b.low) }
    in
    let lines = List.filter ~f:(fun x -> String.length x > 0) input in
    let seeds = parse_seeds (List.nth_exn lines 0) in
    let maps = parse_maps (List.tl_exn lines) None [] |> List.map ~f:sort_map in
    { seeds; maps }
  ;;

  (* Checking each seed in the range individually would be too expensive in part 2,
     so instead we pass the full range of possible seeds through the first map
     to determine the range of possible outputs. Then, those ranges are passed through
     the next map, and so on until we get the range of possible locations, at which point
     we can just take the smallest location. *)
  let solve ~seed_ranges ~maps =
    let under low range =
      if low < range.low
      then (
        let new_range = { low; high = range.low - 1; transform = 0 } in
        let new_low = range.low + 1 in
        [ new_range ], new_low)
      else [], low
    in
    let overlapping low high range input_range =
      if low >= range.low && low <= high
      then (
        let new_high = min range.high input_range.high in
        let new_range =
          { low = max range.low input_range.low + range.transform
          ; high = new_high + range.transform
          ; transform = 0
          }
        in
        let new_low = new_high + 1 in
        [ new_range ], new_low)
      else [], low
    in
    let after low high map =
      let highest_in_ranges = (List.last_exn map.ranges).high in
      if high > highest_in_ranges
      then (
        let new_range = { low = max highest_in_ranges low + 1; high; transform = 0 } in
        [ new_range ])
      else []
    in
    let pass_ranges_through_map ranges map =
      List.concat_map ranges ~f:(fun input_range ->
        let high = input_range.high in
        let new_ranges, low =
          List.fold_left
            map.ranges
            ~init:([], input_range.low)
            ~f:(fun (ranges, low) range ->
              if high < range.low || low > range.high
              then ranges, low
              else (
                let range_under, low = under low range in
                let overlapping_range, low = overlapping low high range input_range in
                ranges @ range_under @ overlapping_range, low))
        in
        let after_ranges = after low high map in
        let resulting_ranges = after_ranges @ new_ranges in
        if List.length resulting_ranges = 0 then [ input_range ] else resulting_ranges)
    in
    let results =
      List.map seed_ranges ~f:(fun seed_range ->
        let thing =
          List.fold_left maps ~init:[ seed_range ] ~f:pass_ranges_through_map
          |> List.map ~f:(fun range -> range.low)
          |> List.min_elt ~compare:Int.compare
          |> Option.value_exn
        in
        thing)
    in
    List.min_elt ~compare:Int.compare results |> Option.value_exn
  ;;

  let solve_1 input =
    let { seeds; maps } = parse input in
    let seed_ranges =
      List.map seeds ~f:(fun seed -> { low = seed; high = seed; transform = 0 })
    in
    solve ~seed_ranges ~maps
  ;;

  let solve_2 input =
    let { seeds; maps } = parse input in
    let seed_ranges =
      Util.list_to_pairs seeds
      |> List.map ~f:(fun (low, span) -> { low; high = low + span - 1; transform = 0 })
    in
    solve ~seed_ranges ~maps
  ;;
end

module Day_6 = struct
  type race =
    { time : int
    ; distance : int
    }

  let solve race =
    let time = float_of_int race.time in
    let distance = float_of_int race.distance in
    let discriminant = sqrt ((time ** 2.0) -. (4.0 *. distance)) in
    let solution_1 = Float.round_down ((time -. discriminant) /. 2.0) in
    let solution_2 = Float.round_down ((time +. discriminant) /. 2.0) in
    solution_2 -. solution_1 |> int_of_float |> abs
  ;;

  let solution_1 =
    Util.measure_ns
      (fun _ ->
        let input =
          [ { time = 49; distance = 298 }
          ; { time = 78; distance = 1185 }
          ; { time = 79; distance = 1066 }
          ; { time = 80; distance = 1181 }
          ]
        in
        List.map ~f:solve input |> Util.product)
      ()
  ;;

  let solution_2 =
    Util.measure_ns
      (fun _ ->
        let input = [ { time = 49787980; distance = 298118510661181 } ] in
        List.map ~f:solve input |> Util.sum)
      ()
  ;;
end

module Day_7 = struct
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
end

module Day_8 = struct
  type instruction =
    | Right
    | Left

  let parse_instructions str =
    let parse_direction char =
      match char with
      | 'R' -> Some Right
      | 'L' -> Some Left
      | _ -> None
    in
    String.to_list str |> List.map ~f:parse_direction |> List.filter_map ~f:(fun x -> x)
  ;;

  let parse_graph lines =
    let clean_string string = Str.global_replace (Str.regexp "[\\(|\\)|,|=]") "" string in
    let parse_graph_line line =
      let list = line |> clean_string |> String.split_on_chars ~on:[ ' ' ] in
      let name = List.nth_exn list 0 in
      let left = List.nth_exn list 2 in
      let right = List.nth_exn list 3 in
      name, (left, right)
    in
    List.map ~f:parse_graph_line lines |> String.Map.of_alist_exn
  ;;

  let find name instruction graph =
    let destinations = Map.find_exn graph name in
    match instruction with
    | Left -> fst destinations
    | Right -> snd destinations
  ;;

  let solve_1 input =
    let solve graph instructions =
      let rec traverse name count =
        let index = count mod List.length instructions in
        let instruction = List.nth_exn instructions index in
        let target = find name instruction graph in
        if String.( = ) target "ZZZ" then count + 1 else traverse target (count + 1)
      in
      traverse "AAA" 0
    in
    match input with
    | raw_instructions :: _ :: raw_graph ->
      let instructions = parse_instructions raw_instructions in
      let graph = parse_graph raw_graph in
      solve graph instructions
    | _ -> failwith "invalid input for day 8"
  ;;

  let solve_2 input =
    let solve graph instructions =
      let starting_points =
        Map.to_alist graph
        |> List.map ~f:fst
        |> List.filter ~f:(String.is_suffix ~suffix:"A")
      in
      (* Number of steps that need to be taken before getting to a point that ends with Z *)
      let traverse name =
        let rec traverse_aux name count =
          let index = count mod List.length instructions in
          let instruction = List.nth_exn instructions index in
          let target = find name instruction graph in
          let is_finished = String.is_suffix ~suffix:"Z" name in
          if is_finished then count else traverse_aux target (count + 1)
        in
        traverse_aux name 0
      in
      List.map ~f:traverse starting_points |> Util.lcm_list
    in
    match input with
    | raw_instructions :: _ :: raw_graph ->
      let instructions = parse_instructions raw_instructions in
      let graph = parse_graph raw_graph in
      solve graph instructions
    | _ -> failwith "invalid input for day 8"
  ;;
end

module Day_9 = struct
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
      if List.for_all ~f:(( = ) 0) input
      then acc
      else find_lists_aux result (result :: acc)
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
end

module Day_10 = struct
  type connected_tile =
    { id : string
    ; symbol : char
    ; at : point
    ; connects_to : point list
    }

  type start_tile =
    { id : string
    ; symbol : char
    ; at : point
    }

  type empty_tile =
    { id : string
    ; symbol : char
    ; at : point
    }

  type tile =
    | Start of start_tile
    | Empty of empty_tile
    | Connected of connected_tile

  let parse_tile symbol at =
    let row = fst at in
    let col = snd at in
    let id = show_point at in
    match symbol with
    | '|' -> Connected { id; symbol; at; connects_to = [ row - 1, col; row + 1, col ] }
    | '-' -> Connected { id; symbol; at; connects_to = [ row, col - 1; row, col + 1 ] }
    | 'L' -> Connected { id; symbol; at; connects_to = [ row - 1, col; row, col + 1 ] }
    | 'J' -> Connected { id; symbol; at; connects_to = [ row - 1, col; row, col - 1 ] }
    | '7' -> Connected { id; symbol; at; connects_to = [ row + 1, col; row, col - 1 ] }
    | 'F' -> Connected { id; symbol; at; connects_to = [ row + 1, col; row, col + 1 ] }
    | '.' -> Empty { id; symbol; at }
    | 'S' -> Start { id; symbol; at }
    | c -> failwith ("invalid symbol in graph: " ^ String.make 1 c)
  ;;

  let parse lines =
    let parse_row line row =
      let array = String.to_array line in
      Array.mapi ~f:(fun col char -> parse_tile char (row, col)) array
    in
    Array.mapi ~f:(fun row line -> parse_row line row) lines
  ;;

  let get_start graph =
    graph
    |> Array.find_map ~f:(fun row ->
      Array.find_map
        ~f:(fun tile ->
          match tile with
          | Start _ -> Some tile
          | _ -> None)
        row)
    |> Option.value_exn
  ;;

  let tile_symbol tile =
    match tile with
    | Start n -> n.symbol
    | Connected n -> n.symbol
    | Empty n -> n.symbol
  ;;

  let tile_at graph point =
    let number_of_rows = Array.length graph in
    let number_of_columns = Array.length (Array.get graph 0) in
    let row_out_of_bounds = fst point < 0 || fst point >= number_of_rows in
    let col_out_of_bounds = snd point < 0 || snd point >= number_of_columns in
    if row_out_of_bounds || col_out_of_bounds
    then None
    else (
      let row = Array.get graph (fst point) in
      Some (Array.get row (snd point)))
  ;;

  let tile_location tile =
    match tile with
    | Start n -> n.at
    | Connected n -> n.at
    | Empty n -> n.at
  ;;

  let tile_id tile =
    match tile with
    | Start n -> n.id
    | Connected n -> n.id
    | Empty n -> n.id
  ;;

  let equal_tile tile_1 tile_2 =
    let id_1 = tile_id tile_1 in
    let id_2 = tile_id tile_2 in
    String.( = ) id_1 id_2
  ;;

  let find_loop graph =
    let neighbors_of tile =
      match tile with
      | Start n -> neighbors_of_point n.at |> List.filter_map ~f:(tile_at graph)
      | Connected n -> n.connects_to |> List.filter_map ~f:(tile_at graph)
      | Empty _ -> []
    in
    let rec find_cycle tile previous visited =
      match tile with
      | Start _ -> [ visited ]
      | Empty _ -> [ String.Set.empty ]
      | Connected _ ->
        let neighbors =
          neighbors_of tile
          |> List.filter ~f:(fun new_tile -> not (equal_tile previous new_tile))
        in
        let tile_key = tile_id tile in
        neighbors
        |> List.concat_map ~f:(fun new_tile ->
          find_cycle new_tile tile (Set.add visited tile_key))
    in
    let start = get_start graph in
    let visited = String.Set.singleton (tile_id start) in
    let paths : String.Set.t list =
      neighbors_of start
      |> List.concat_map ~f:(fun new_tile -> find_cycle new_tile start visited)
    in
    let loops : String.Set.t list =
      paths |> List.filter ~f:(fun x -> not (Set.is_empty x))
    in
    loops |> Util.list_max_by Set.length
  ;;

  let solve_2 graph loop =
    let is_in_loop tile = Set.mem loop (tile_id tile) in
    let all_tiles = graph |> Array.to_list |> Array.concat |> Array.to_list in
    (* Shoot a ray from the current tile outwards and count the the number
       of intersections with the loop. If this number is odd, the tile is
       inside the loop *)
    let is_inside_loop tile =
      let row, col = tile_location tile in
      let rec find_ray_path (row, col) acc =
        let tile = tile_at graph (row, col) in
        match tile with
        | None -> acc
        | Some tile -> find_ray_path (row + 1, col + 1) (tile :: acc)
      in
      let is_intersection tile =
        let symbol = tile_symbol tile in
        (* NOTE: this assumes 'S' is '7' or 'L', which is true in my input but not generally *)
        is_in_loop tile
        && Char.( <> ) symbol '7'
        && Char.( <> ) symbol 'L'
        && Char.( <> ) symbol 'S'
      in
      let ray_path = find_ray_path (row, col) [] in
      let number_of_intersections =
        ray_path |> List.filter ~f:is_intersection |> List.length
      in
      Util.is_odd number_of_intersections
    in
    let tiles_inside_loop _ =
      all_tiles
      |> List.filter ~f:(fun tile -> not (is_in_loop tile))
      |> List.filter ~f:is_inside_loop
    in
    let tiles_in_loop = tiles_inside_loop () in
    List.length tiles_in_loop
  ;;

  let solve_1 loop = Set.length loop / 2
end

let () =
  print_endline "";
  let day_1 = Util.read_file "inputs/day1.txt" |> List.map ~f:String.to_list in
  print_endline "Day 1";
  let day_1_part_1 = Util.measure_ns Day_1.solve_1 day_1 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_1_part_1 ~expected:54450);
  let day_1_part_2 = Util.measure_ns Day_1.solve_2 day_1 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_1_part_2 ~expected:54265);
  print_endline "";
  (* / *)
  let day_2 = Util.read_file "inputs/day2.txt" in
  print_endline "Day 2";
  let day_2_part_1 = Util.measure_ns Day_2.solve_1 day_2 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_2_part_1 ~expected:2164);
  let day_2_part_2 = Util.measure_ns Day_2.solve_2 day_2 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_2_part_2 ~expected:69929);
  print_endline "";
  (* / *)
  let day_3 = Util.read_file "inputs/day3.txt" in
  print_endline "Day 3";
  let day_3_part_1 = Util.measure_ns Day_3.solve_1 day_3 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_3_part_1 ~expected:549908);
  let day_3_part_2 = Util.measure_ns Day_3.solve_2 day_3 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_3_part_2 ~expected:81166799);
  print_endline "";
  (* / *)
  let day_4 = Util.read_file "inputs/day4.txt" in
  print_endline "Day 4";
  let day_4_part_1 = Util.measure_ns Day_4.solve_1 day_4 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_4_part_1 ~expected:20829);
  let day_4_part_2 = Util.measure_ns Day_4.solve_2 day_4 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_4_part_2 ~expected:12648035);
  print_endline "";
  (* / *)
  let day_5 = Util.read_file "inputs/day5.txt" in
  print_endline "Day 5";
  let day_5_part_1 = Util.measure_ns Day_5.solve_1 day_5 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_5_part_1 ~expected:323142486);
  let day_5_part_2 = Util.measure_ns Day_5.solve_2 day_5 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_5_part_2 ~expected:79874951);
  print_endline "";
  (* / *)
  print_endline "Day 6";
  print_endline ("Part 1 " ^ Util.to_result ~result:Day_6.solution_1 ~expected:2269432);
  print_endline ("Part 2 " ^ Util.to_result ~result:Day_6.solution_2 ~expected:35865985);
  print_endline "";
  (* / *)
  let day_7 = Util.read_file "inputs/day7.txt" in
  print_endline "Day 7";
  let day_7_part_1 = Util.measure_ns Day_7.solve_1 day_7 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_7_part_1 ~expected:250370104);
  let day_7_part_2 = Util.measure_ns Day_7.solve_2 day_7 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_7_part_2 ~expected:251735672);
  print_endline "";
  (* / *)
  let day_8 = Util.read_file "inputs/day8.txt" in
  print_endline "Day 8";
  let day_8_part_1 = Util.measure_ns Day_8.solve_1 day_8 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_8_part_1 ~expected:19783);
  let day_8_part_2 = Util.measure_ns Day_8.solve_2 day_8 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_8_part_2 ~expected:9177460370549);
  print_endline "";
  (* / *)
  print_endline "Day 9";
  let day_9 = Util.read_file "inputs/day9.txt" in
  let day_9_part_1 = Util.measure_ns Day_9.solve_1 day_9 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_9_part_1 ~expected:1842168671);
  let day_9_part_2 = Util.measure_ns Day_9.solve_2 day_9 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_9_part_2 ~expected:903);
  print_endline "";
  (* / *)
  print_endline "Day 10";
  let day_10 = Util.read_file "inputs/day10.txt" |> Array.of_list in
  let graph = Util.measure_ns Day_10.parse day_10 in
  let loop = Util.measure_ns Day_10.find_loop graph.value in
  let day_10_part_1 = Util.measure_ns Day_10.solve_1 loop.value in
  let day_10_part_1_full =
    Util.make_measurement
      ~value:day_10_part_1.value
      ~time_ns:(day_10_part_1.time_ns +. loop.time_ns +. graph.time_ns)
  in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_10_part_1_full ~expected:6867);
  let day_10_part_2 = Util.measure_ns (Day_10.solve_2 graph.value) loop.value in
  let day_10_part_2_full =
    Util.make_measurement
      ~value:day_10_part_2.value
      ~time_ns:(day_10_part_2.time_ns +. loop.time_ns +. graph.time_ns)
  in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_10_part_2_full ~expected:595);
  print_endline ""
;;
