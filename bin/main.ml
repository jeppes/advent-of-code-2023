open Core
open Aoc2023

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
  print_endline "";
  (* / *)
  print_endline "Day 11";
  let day_11 = Util.read_file "inputs/day11.txt" in
  let day_11_part_1 = Util.measure_ns Day_11.solve_1 day_11 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_11_part_1 ~expected:9742154);
  let day_11_part_2 = Util.measure_ns Day_11.solve_2 day_11 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_11_part_2 ~expected:411142919886);
  print_endline "";
  (* / *)
  print_endline "Day 12";
  let day_12 = Util.read_file "inputs/day12.txt" in
  let day_12_part_1 = Util.measure_ns Day_12.solve_1 day_12 in
  print_endline ("Part 1 " ^ Util.to_result ~result:day_12_part_1 ~expected:6488);
  let day_12_part_2 = Util.measure_ns Day_12.solve_2 day_12 in
  print_endline ("Part 2 " ^ Util.to_result ~result:day_12_part_2 ~expected:815364548481);
  print_endline ""
;;
