open Core
open Aoc2023

let day_1 _ =
  let day_1 = Util.read_file "inputs/day1.txt" |> List.map ~f:String.to_list in
  print_endline "Day 1";
  let day_1_part_1 = Util.measure_ns Day_1.solve_1 day_1 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_1_part_1 ~expected:54450);
  let day_1_part_2 = Util.measure_ns Day_1.solve_2 day_1 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_1_part_2 ~expected:54265)
;;

let day_2 _ =
  let day_2 = Util.read_file "inputs/day2.txt" in
  print_endline "Day 2";
  let day_2_part_1 = Util.measure_ns Day_2.solve_1 day_2 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_2_part_1 ~expected:2164);
  let day_2_part_2 = Util.measure_ns Day_2.solve_2 day_2 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_2_part_2 ~expected:69929)
;;

let day_3 _ =
  let day_3 = Util.read_file "inputs/day3.txt" in
  print_endline "Day 3";
  let day_3_part_1 = Util.measure_ns Day_3.solve_1 day_3 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_3_part_1 ~expected:549908);
  let day_3_part_2 = Util.measure_ns Day_3.solve_2 day_3 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_3_part_2 ~expected:81166799)
;;

let day_4 _ =
  let day_4 = Util.read_file "inputs/day4.txt" in
  print_endline "Day 4";
  let day_4_part_1 = Util.measure_ns Day_4.solve_1 day_4 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_4_part_1 ~expected:20829);
  let day_4_part_2 = Util.measure_ns Day_4.solve_2 day_4 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_4_part_2 ~expected:12648035)
;;

let day_5 _ =
  let day_5 = Util.read_file "inputs/day5.txt" in
  print_endline "Day 5";
  let day_5_part_1 = Util.measure_ns Day_5.solve_1 day_5 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_5_part_1 ~expected:323142486);
  let day_5_part_2 = Util.measure_ns Day_5.solve_2 day_5 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_5_part_2 ~expected:79874951)
;;

let day_6 _ =
  print_endline "Day 6";
  print_endline ("Part 1 " ^ Util.to_result ~result:Day_6.solution_1 ~expected:2269432);
  print_endline ("Part 2 " ^ Util.to_result ~result:Day_6.solution_2 ~expected:35865985)
;;

let day_7 _ =
  let day_7 = Util.read_file "inputs/day7.txt" in
  print_endline "Day 7";
  let day_7_part_1 = Util.measure_ns Day_7.solve_1 day_7 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_7_part_1 ~expected:250370104);
  let day_7_part_2 = Util.measure_ns Day_7.solve_2 day_7 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_7_part_2 ~expected:251735672)
;;

let day_8 _ =
  let day_8 = Util.read_file "inputs/day8.txt" in
  print_endline "Day 8";
  let day_8_part_1 = Util.measure_ns Day_8.solve_1 day_8 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_8_part_1 ~expected:19783);
  let day_8_part_2 = Util.measure_ns Day_8.solve_2 day_8 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_8_part_2 ~expected:9177460370549)
;;

let day_9 _ =
  let day_9 = Util.read_file "inputs/day9.txt" in
  let day_9_part_1 = Util.measure_ns Day_9.solve_1 day_9 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_9_part_1 ~expected:1842168671);
  let day_9_part_2 = Util.measure_ns Day_9.solve_2 day_9 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_9_part_2 ~expected:903)
;;

let day_10 _ =
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
  print_endline ("Part 2 " ^ Util.to_result ~result:day_10_part_2_full ~expected:595)
;;

let day_11 _ =
  print_endline "Day 11";
  let day_11 = Util.read_file "inputs/day11.txt" in
  let day_11_part_1 = Util.measure_ns Day_11.solve_1 day_11 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_11_part_1 ~expected:9742154);
  let day_11_part_2 = Util.measure_ns Day_11.solve_2 day_11 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_11_part_2 ~expected:411142919886)
;;

let day_12 _ =
  print_endline "Day 12";
  let day_12 = Util.read_file "inputs/day12.txt" in
  let day_12_part_1 = Util.measure_ns Day_12.solve_1 day_12 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_12_part_1 ~expected:6488);
  let day_12_part_2 = Util.measure_ns Day_12.solve_2 day_12 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_12_part_2 ~expected:815364548481)
;;

let day_13 _ =
  print_endline "Day 13";
  let day_13 = Util.read_file "inputs/day13.txt" in
  let day_13_part_1 = Util.measure_ns Day_13.solve_1 day_13 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_13_part_1 ~expected:34821);
  let day_13_part_2 = Util.measure_ns Day_13.solve_2 day_13 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_13_part_2 ~expected:36919)
;;

let day_14 _ =
  print_endline "Day 14";
  let day_14 = Util.read_file "inputs/day14.txt" in
  let day_14_part_1 = Util.measure_ns Day_14.solve_1 day_14 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_14_part_1 ~expected:103333);
  let day_14_part_2 = Util.measure_ns Day_14.solve_2 day_14 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_14_part_2 ~expected:97241)
;;

let day_15 _ =
  print_endline "Day 15";
  let day_15 = Util.read_file "inputs/day15.txt" in
  let day_15_part_1 = Util.measure_ns Day_15.solve_1 day_15 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_15_part_1 ~expected:509784);
  let day_15_part_2 = Util.measure_ns Day_15.solve_2 day_15 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_15_part_2 ~expected:230197)
;;

let day_16 _ =
  print_endline "Day 16";
  let day_16 = Util.read_file "inputs/day16.txt" in
  let day_16_part_1 = Util.measure_ns Day_16.solve_1 day_16 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_16_part_1 ~expected:7728);
  let day_16_part_2 = Util.measure_ns Day_16.solve_2 day_16 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_16_part_2 ~expected:8061)
;;

let day_17 _ =
  print_endline "Day 17";
  let day_17 = Util.read_file "inputs/day17.txt" in
  let day_17_part_1 = Util.measure_ns Day_17.solve_1 day_17 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_17_part_1 ~expected:1039);
  let day_17_part_2 = Util.measure_ns Day_17.solve_2 day_17 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_17_part_2 ~expected:1201)
;;

let day_18 _ =
  print_endline "Day 18";
  let day_18 = Util.read_file "inputs/day18.txt" in
  let day_18_part_1 = Util.measure_ns Day_18.solve_1 day_18 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_18_part_1 ~expected:58550);
  let day_18_part_2 = Util.measure_ns Day_18.solve_2 day_18 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_18_part_2 ~expected:47452118468566)
;;

let run_all () =
  let days =
    [ day_1
    ; day_2
    ; day_3
    ; day_4
    ; day_5
    ; day_6
    ; day_7
    ; day_8
    ; day_9
    ; day_10
    ; day_11
    ; day_12
    ; day_13
    ; day_14
    ; day_15
    ; day_16
    ; day_17
    ; day_18
    ]
  in
  List.iter days ~f:(fun day ->
    day ();
    print_endline "")
;;

let () = run_all ()
