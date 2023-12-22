open Core

type position =
  { x : int
  ; y : int
  ; z : int
  }

type brick =
  { index : int
  ; start : position
  ; end_ : position
  }

let parse lines =
  let parse_line index line =
    let split =
      String.split_on_chars line ~on:[ ','; '~' ] |> List.map ~f:Int.of_string
    in
    match split with
    | [ x1; y1; z1; x2; y2; z2 ] ->
      let start = { x = x1; y = y1; z = z1 } in
      let end_ = { x = x2; y = y2; z = z2 } in
      { index; start; end_ }
    | _ -> failwith "Invalid line"
  in
  List.mapi lines ~f:parse_line
;;

let make_brick_map max_x max_y max_z =
  Array.init (max_z + 1) ~f:(fun _ ->
    Array.make_matrix ~dimx:(max_x + 1) ~dimy:(max_y + 1) (-1))
;;

let move_bricks_down (bricks : brick list) =
  let max_x = List.map bricks ~f:(fun b -> b.end_.x) |> Util.list_max in
  let max_y = List.map bricks ~f:(fun b -> b.end_.y) |> Util.list_max in
  let max_z = List.map bricks ~f:(fun b -> b.end_.z) |> Util.list_max in
  let brick_map = make_brick_map max_x max_y max_z in
  List.iter bricks ~f:(fun brick ->
    for x = brick.start.x to brick.end_.x do
      for y = brick.start.y to brick.end_.y do
        for z = brick.start.z to brick.end_.z do
          brick_map.(z).(x).(y) <- brick.index
        done
      done
    done);
  (* Sort bricks by lowest z point *)
  let sorted_bricks =
    List.sort bricks ~compare:(fun brick_1 brick_2 ->
      Int.compare brick_1.start.z brick_2.start.z)
  in
  let moved_bricks =
    List.map sorted_bricks ~f:(fun brick ->
      let { start; end_; _ } = brick in
      let to_x = if start.z <> end_.z then start.x else end_.x in
      let to_y = if start.z <> end_.z then start.y else end_.y in
      (* Find the highest point where the brick can land *)
      let move_distance =
        List.range 1 start.z
        |> List.rev
        |> List.take_while ~f:(fun z ->
          List.for_all
            (List.range start.x (to_x + 1))
            ~f:(fun x ->
              List.for_all
                (List.range start.y (to_y + 1))
                ~f:(fun y -> brick_map.(z).(x).(y) = -1)))
        |> List.length
      in
      let top_z = start.z - move_distance in
      if start.z <> top_z
      then (
        (* Erase the old brick *)
        for x = start.x to end_.x do
          for y = start.y to end_.y do
            for z = start.z to end_.z do
              brick_map.(z).(x).(y) <- -1
            done
          done
        done;
        (* Place the new brick if it moved *)
        for x = start.x to end_.x do
          for y = start.y to end_.y do
            for z = top_z to top_z + (end_.z - start.z) do
              brick_map.(z).(x).(y) <- brick.index
            done
          done
        done);
      let new_start = { x = start.x; y = start.y; z = top_z } in
      let new_end = { x = end_.x; y = end_.y; z = top_z + (end_.z - start.z) } in
      { brick with start = new_start; end_ = new_end })
  in
  (* For each brick, find out which bricks are holding it up *)
  let supported_by_map =
    List.map moved_bricks ~f:(fun { start; end_; index } ->
      let to_x = if start.z = end_.z then end_.x else start.x in
      let to_y = if start.z = end_.z then end_.y else start.y in
      let supported_by_set = ref Int.Set.empty in
      for x = start.x to to_x do
        for y = start.y to to_y do
          let z = start.z - 1 in
          if brick_map.(z).(x).(y) <> -1 && brick_map.(z).(x).(y) <> index
          then supported_by_set := Set.add !supported_by_set brick_map.(z).(x).(y)
        done
      done;
      index, !supported_by_set)
    |> Int.Map.of_alist_exn
  in
  (* For each brick, find out which bricks it is supporting *)
  let supporting_map =
    List.concat_map (Map.keys supported_by_map) ~f:(fun key ->
      let supporting_brick = key in
      let bricks_supported = Map.find_exn supported_by_map key in
      bricks_supported
      |> Set.to_list
      |> List.map ~f:(fun brick_supported -> brick_supported, supporting_brick))
    |> Int.Map.of_alist_multi
  in
  (*
     If brick A supports brick B, brick A can only be removed if another brick also supports brick B.
  *)
  let bricks_to_remove =
    List.filter moved_bricks ~f:(fun brick ->
      let supporting_bricks =
        Map.find supporting_map brick.index |> Option.value ~default:[]
      in
      List.for_all supporting_bricks ~f:(fun supporting_brick ->
        let count =
          Map.find_exn supported_by_map supporting_brick |> Set.count ~f:(fun _ -> true)
        in
        count > 1))
    |> List.map ~f:(fun brick -> brick.index)
  in
  bricks_to_remove, supported_by_map, supporting_map
;;

let solve_1 input =
  let bricks = parse input in
  let bricks_to_remove, _, _ = move_bricks_down bricks in
  List.length bricks_to_remove
;;

let solve_2 input =
  let bricks = parse input in
  let bricks_to_remove, supported_by_map, supporting_map = move_bricks_down bricks in
  let rec count_bricks index (destroyed : Int.Set.t) =
    let supporting = Map.find supporting_map index |> Option.value ~default:[] in
    let supported_bricks_that_would_fall =
      List.filter supporting ~f:(fun brick' ->
        let supporting_brick' = Map.find_exn supported_by_map brick' |> Set.to_list in
        let supporting_brick'_that_still_exist =
          List.filter supporting_brick' ~f:(fun supporter ->
            not (Set.mem destroyed supporter))
        in
        supporting_brick'_that_still_exist |> List.length = 0)
    in
    let new_destroyed : Int.Set.t =
      Set.union destroyed (Int.Set.of_list supported_bricks_that_would_fall)
    in
    List.fold_left
      supported_bricks_that_would_fall
      ~init:new_destroyed
      ~f:(fun acc index -> Set.union acc (count_bricks index acc))
  in
  bricks
  |> List.map ~f:(fun brick -> brick.index)
  |> List.filter ~f:(fun index -> not (List.mem bricks_to_remove index ~equal:( = )))
  |> List.map ~f:(fun index ->
    let total_bricks_that_would_fall =
      count_bricks index (Int.Set.singleton index) |> Set.count ~f:(fun _ -> true)
    in
    (* Don't count the disintegrated brick *)
    total_bricks_that_would_fall - 1)
  |> Util.sum
;;
