open Core

type pulse =
  | High
  | Low
[@@deriving show, eq]

let show_pulse pulse =
  match pulse with
  | High -> "high"
  | Low -> "low"
;;

type flip_flop =
  { name : string
  ; outputs : string list
  ; is_on : bool
  }
[@@deriving show, eq]

type conjunction =
  { name : string
  ; inputs : (string * pulse) list
  ; outputs : string list
  }
[@@deriving show, eq]

type gate =
  | Flip_Flop of flip_flop
  | Conjunction of conjunction
[@@deriving show, eq]

let gate_outputs gate =
  match gate with
  | Flip_Flop flip_flop -> flip_flop.outputs
  | Conjunction conjunction -> conjunction.outputs
;;

let gate_name gate =
  match gate with
  | Flip_Flop flip_flop -> flip_flop.name
  | Conjunction conjunction -> conjunction.name
;;

let parse (input : string list) =
  let broadcaster_string =
    List.filter input ~f:(fun s -> String.is_prefix s ~prefix:"broadcaster")
    |> List.hd_exn
    |> Util.cut_off "broadcaster -> "
  in
  let start_gate_names =
    String.split_on_chars broadcaster_string ~on:[ ','; ' ' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let flip_flops =
    List.filter input ~f:(fun s -> String.is_prefix s ~prefix:"%")
    |> List.map ~f:(Util.cut_off "%")
    |> List.map ~f:(fun s ->
      let parts =
        s
        |> String.split_on_chars ~on:[ ' '; '-'; '>'; ',' ]
        |> List.filter ~f:(fun s -> not (String.is_empty s))
      in
      let name = List.nth_exn parts 0 in
      let outputs = List.tl_exn parts in
      name, Flip_Flop { name; outputs; is_on = false })
  in
  let conjunctions_without_inputs =
    List.filter input ~f:(fun s -> String.is_prefix s ~prefix:"&")
    |> List.map ~f:(Util.cut_off "&")
    |> List.map ~f:(fun s ->
      let parts =
        s
        |> String.split_on_chars ~on:[ ' '; '-'; '>'; ',' ]
        |> List.filter ~f:(fun s -> not (String.is_empty s))
      in
      let name = List.nth_exn parts 0 in
      (* let inputs = List.nth_exn parts 1 in *)
      let outputs = List.tl_exn parts in
      name, Conjunction { name; inputs = []; outputs })
  in
  let gates_by_output =
    flip_flops @ conjunctions_without_inputs
    |> List.concat_map ~f:(fun (_, gate) ->
      gate_outputs gate |> List.map ~f:(fun output -> output, gate_name gate))
    |> String.Map.of_alist_multi
  in
  let conjunctions =
    List.map conjunctions_without_inputs ~f:(fun (name, gate) ->
      match gate with
      | Flip_Flop _ -> failwith "expected conjunction"
      | Conjunction gate ->
        let inputs = Map.find_exn gates_by_output name |> List.map ~f:(fun n -> n, Low) in
        name, Conjunction { gate with inputs })
  in
  let gates = String.Map.of_alist_exn (conjunctions @ flip_flops) in
  start_gate_names, gates, gates_by_output
;;

type source =
  | Gate of gate
  | Broadcast
  | Output of string
[@@deriving show, eq]

let source_name gate =
  match gate with
  | Gate gate -> gate_name gate
  | Broadcast -> "broadcast"
  | Output string -> string
;;

let input_name input =
  match input with
  | Gate gate -> gate_name gate
  | Broadcast -> "broadcast"
  | Output string -> string
;;

let gates_state gates =
  Hashtbl.to_alist gates |> List.map ~f:(fun (_, gate) -> show_gate gate) |> String.concat
;;

type pulses =
  { mutable high : int
  ; mutable low : int
  }

let get_gate_exn gates name = Hashtbl.find_exn gates name
let get_gate gates name = Hashtbl.find gates name
let write_gate gates name gate = Hashtbl.set gates ~key:name ~data:gate

let press_button stopping_points start_gate_names gates pulses =
  pulses.low <- pulses.low + 1;
  let start_gates = List.map start_gate_names ~f:(fun name -> get_gate_exn gates name) in
  let queue = Queue.create () in
  List.iter start_gates ~f:(fun gate -> Queue.enqueue queue (Gate gate, Low, Broadcast));
  while not (Queue.is_empty queue) do
    let input, pulse, source = Queue.dequeue_exn queue in
    if equal_pulse pulse High
    then pulses.high <- pulses.high + 1
    else pulses.low <- pulses.low + 1;
    match input with
    | Output _ -> ()
    | Broadcast -> failwith "Shouldn't be possible to reach broadcast here"
    | Gate gate ->
      (match gate with
       | Flip_Flop gate ->
         (match pulse with
          | High -> ()
          | Low ->
            let new_gate = { gate with is_on = not gate.is_on } in
            let new_pulse = if new_gate.is_on then High else Low in
            let new_instructions =
              List.map gate.outputs ~f:(fun output ->
                let to_gate =
                  get_gate gates output
                  |> Option.map ~f:(fun g -> Gate g)
                  |> Option.value ~default:(Output output)
                in
                let pulse = new_pulse in
                let source = Gate (Flip_Flop new_gate) in
                to_gate, pulse, source)
            in
            write_gate gates gate.name (Flip_Flop new_gate);
            Queue.enqueue_all queue new_instructions)
       | Conjunction gate ->
         let new_inputs =
           List.map gate.inputs ~f:(fun (name, old_pulse) ->
             if String.( = ) name (source_name source)
             then name, pulse
             else name, old_pulse)
         in
         let new_gate = { gate with inputs = new_inputs } in
         let all_inputs_high =
           List.for_all new_inputs ~f:(fun (_, pulse) -> equal_pulse pulse High)
         in
         let output_pulse = if all_inputs_high then Low else High in
         let new_instructions =
           List.map gate.outputs ~f:(fun output ->
             let to_gate =
               get_gate gates output
               |> Option.map ~f:(fun g -> Gate g)
               |> Option.value ~default:(Output output)
             in
             to_gate, output_pulse, Gate (Conjunction new_gate))
         in
         write_gate gates gate.name (Conjunction new_gate);
         Queue.enqueue_all queue new_instructions)
  done
;;

let solve_1 input =
  let start_gate_names, gates_map, gates_by_output = parse input in
  let second_to_last_gates = Map.find_exn gates_by_output "rx" in
  if List.length second_to_last_gates > 1
  then failwith "expected only one second to last gate";
  let gates = Hashtbl.create (module String) in
  List.iter (Map.to_alist gates_map) ~f:(fun (name, gate) ->
    Hashtbl.set gates ~key:name ~data:gate);
  let pulses = { high = 0; low = 0 } in
  for i = 0 to 999 do
    press_button i start_gate_names gates pulses
  done;
  let result = pulses.high * pulses.low in
  result
;;
