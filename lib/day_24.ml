open Core

let parse_line line =
  let split =
    String.split_on_chars line ~on:[ ','; ' '; '@' ]
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun x -> not (String.is_empty x))
    |> List.map ~f:float_of_string
  in
  match split with
  | [ x; y; z; dx; dy; dz ] -> (x, y, z), (dx, dy, dz)
  | _ -> failwith "could not parse"
;;

let solve_1 input =
  let boundary = 200000000000000.0, 400000000000000.0 in
  let sum = ref 0 in
  List.iteri input ~f:(fun i line1 ->
    input
    |> List.filteri ~f:(fun j _ -> j > i)
    |> List.iter ~f:(fun line2 ->
      let (x1, y1, _), (dx1, dy1, _) = parse_line line1 in
      let (x2, y2, _), (dx2, dy2, _) = parse_line line2 in
      let result =
        Owl.solve_linear_equations
          [| [| dx1; -.dx2 |]; [| dy1; -.dy2 |] |]
          [| [| x2 -. x1 |]; [| y2 -. y1 |] |]
      in
      match result with
      | None -> ()
      | Some (_, ans) ->
        let factor2 = ans.(1).(0) in
        let x = x2 +. (factor2 *. dx2) in
        let y = y2 +. (factor2 *. dy2) in
        let is_in_past_of_a =
          (if Float.( < ) dx1 0. then Float.( > ) x x1 else Float.( <= ) x x1)
          || if Float.( < ) dy1 0. then Float.( > ) y y1 else Float.( <= ) y y1
        in
        let is_in_past_of_b =
          (if Float.( < ) dx2 0. then Float.( > ) x x2 else Float.( <= ) x x2)
          || if Float.( < ) dy2 0. then Float.( > ) y y2 else Float.( <= ) y y2
        in
        if (not is_in_past_of_a)
           && (not is_in_past_of_b)
           && Float.( >= ) x (fst boundary)
           && Float.( <= ) x (snd boundary)
           && Float.( >= ) y (fst boundary)
           && Float.( <= ) y (snd boundary)
        then sum := !sum + 1));
  !sum
;;
