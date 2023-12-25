open Core

(*
   Linear equation solver
   Taken with slight modifications from: https://blog.ocaml.xyz/algorithm/2023/11/22/gaussian-elimination.html
   Full credit goes to the original source
*)
let linsolve_gauss a b =
  let matrix_shape x = Array.length x, Array.length x.(0) in
  let matrix_copy x = Array.map x ~f:Array.copy in
  let (n, _), (_, m) = matrix_shape a, matrix_shape b in
  let a = matrix_copy a in
  let b = matrix_copy b in
  let icol = ref 0 in
  let irow = ref 0 in
  let dum = ref 0.0 in
  let pivinv = ref 0.0 in
  let indxc = Array.create ~len:n 0 in
  let indxr = Array.create ~len:n 0 in
  let ipiv = Array.create ~len:n 0 in
  (* Main loop over the columns to be reduced. *)
  for i = 0 to n - 1 do
    let big = ref 0.0 in
    (* Outer loop of the search for at pivot element *)
    for j = 0 to n - 1 do
      if ipiv.(j) <> 1
      then
        for k = 0 to n - 1 do
          if ipiv.(k) = 0
          then (
            let v = a.(j).(k) |> Float.abs in
            if Float.( >= ) v !big
            then (
              big := v;
              irow := j;
              icol := k))
        done
    done;
    ipiv.(!icol) <- ipiv.(!icol) + 1;
    if !irow <> !icol
    then (
      for l = 0 to n - 1 do
        let u = a.(!irow).(l) in
        let v = a.(!icol).(l) in
        a.(!icol).(l) <- u;
        a.(!irow).(l) <- v
      done;
      for l = 0 to m - 1 do
        let u = b.(!irow).(l) in
        let v = b.(!icol).(l) in
        b.(!icol).(l) <- u;
        b.(!irow).(l) <- v
      done);
    indxr.(i) <- !irow;
    indxc.(i) <- !icol;
    let p = a.(!icol).(!icol) in
    if Float.( = ) p 0.0 then failwith "linsolve: Singular Matrix";
    pivinv := 1.0 /. p;
    a.(!icol).(!icol) <- 1.0;
    for l = 0 to n - 1 do
      let prev = a.(!icol).(l) in
      a.(!icol).(l) <- prev *. !pivinv
    done;
    for l = 0 to m - 1 do
      let prev = b.(!icol).(l) in
      b.(!icol).(l) <- prev *. !pivinv
    done;
    for ll = 0 to n - 1 do
      if ll <> !icol
      then (
        dum := a.(ll).(!icol);
        a.(ll).(!icol) <- 0.0;
        for l = 0 to n - 1 do
          let p = a.(!icol).(l) in
          let prev = a.(ll).(l) in
          a.(ll).(l) <- prev -. (p *. !dum)
        done;
        for l = 0 to m - 1 do
          let p = b.(!icol).(l) in
          let prev = b.(ll).(l) in
          b.(ll).(l) <- prev -. (p *. !dum)
        done)
    done
  done;
  for l = n - 1 downto 0 do
    if indxr.(l) <> indxc.(l)
    then
      for k = 0 to n - 1 do
        let u = a.(k).(indxr.(l)) in
        let v = a.(k).(indxc.(l)) in
        a.(k).(indxc.(l)) <- u;
        a.(k).(indxr.(l)) <- v
      done
  done;
  a, b
;;

let solve_linear_equations a b =
  try
    let result = linsolve_gauss a b in
    Some result
  with
  | _ -> None
;;
