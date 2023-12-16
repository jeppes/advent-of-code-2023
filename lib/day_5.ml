open Core

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
      List.fold_left maps ~init:[ seed_range ] ~f:pass_ranges_through_map
      |> List.map ~f:(fun range -> range.low)
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn)
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
