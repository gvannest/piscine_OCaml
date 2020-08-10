let main () =
    let a = Array.make 5 "" in
    a.(0) <- "Who is yellow and is waiting? Johnatan" ;
    a.(1) <- "Proute" ;
    a.(2) <- "Exponetial and logarithm are in the restaurant. Who is gonna pay? exp because logarithm neperien" ;
    a.(3) <- "What do you call a hippie's wife? A Mississippi" ;
    a.(4) <- "Where did the computer go dancing? The disc-o" ;
    Random.self_init () ;
    let idx = Random.int 5 in
    print_endline a.(idx)


let () = main ()