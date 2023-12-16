open Core

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
