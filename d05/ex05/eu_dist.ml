let eu_dist (a:float array) (b:float array) : float =
    let diff (x:float) (y:float) = x -. y
    in
    let square (x:float) = x *. x
    in
    let sum (x:float) (y:float) = x +. y
    in
    (* let sroot (n:float) = 
        let p = 0.0001 in
        let rec loop x = match x *. x with
            | m when m > n -. p && m <= n +. p -> m
            | _ -> print_float x ; print_char '\n' ; loop (x +. (n /. x)) /. 2.0
        in
        loop (n /. 2.0)
    in *)
    let toSum = Array.map square (Array.map2 diff a b) in
    let toSroot = Array.fold_left sum 0.0 toSum in
    sqrt toSroot


let () =
  let a = [|1.; 1.; 1.; 1.|] in
  let b = [|2.; 2.; 2.; 2.|] in
  print_string "Eu_dist [|1.; 1.; 1.; 1.|] and [|2.; 2.; 2.; 2.|] = ";
  print_float (eu_dist a b);
  print_endline "";
  print_string "Eu_dist [|4.5; 1.0; (-2.5); 0.4|] and [|2.; 3.; 12.; (-42.)|] = ";
  print_float (eu_dist [|4.5; 1.0; (-2.5); 0.4|] [|2.; 3.; 12.; (-42.)|]);
  print_endline "";
  print_string "Eu_dist [|1.; (-2.)|] and [| 4.; 2.|] = ";
  print_float (eu_dist [|1.; (-2.)|] [| 4.; 2.|]);
  print_endline ""