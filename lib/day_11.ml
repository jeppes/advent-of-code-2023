open Core

let solve lines ~expansion_factor =
  let lists = List.map ~f:String.to_list lines in
  let expanded_row_indices =
    List.concat_mapi lists ~f:(fun row_i row ->
      let is_empty = List.for_all row ~f:(fun x -> Char.( = ) x '.') in
      if is_empty then [ row_i ] else [])
  in
  let expanded_columns_indices =
    let column_indices = List.range 0 (lists |> List.hd_exn |> List.length) in
    List.filter column_indices ~f:(fun col_index ->
      List.for_all lists ~f:(fun row -> Char.( = ) (List.nth_exn row col_index) '.'))
  in
  let galaxies =
    List.concat_mapi lists ~f:(fun row_i row ->
      List.concat_mapi row ~f:(fun col_i col ->
        if Char.( = ) col '#'
        then (
          let expanded_columns =
            List.filter expanded_columns_indices ~f:(fun i -> i < col_i) |> List.length
          in
          let expanded_rows =
            List.filter expanded_row_indices ~f:(fun i -> i < row_i) |> List.length
          in
          [ ( row_i - expanded_rows + (expanded_rows * expansion_factor)
            , col_i - expanded_columns + (expanded_columns * expansion_factor) )
          ])
        else []))
  in
  List.concat_mapi galaxies ~f:(fun i galaxy_1 ->
    List.mapi galaxies ~f:(fun j galaxy_2 ->
      let distance = Util.manhattan_distance galaxy_1 galaxy_2 in
      if j > i then distance else 0))
  |> Util.sum
;;

let solve_1 input = solve input ~expansion_factor:2
let solve_2 input = solve input ~expansion_factor:1000000
