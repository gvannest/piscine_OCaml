let rec iter f x n =
    if n == 0 then x
    else iter f (f x) (n - 1)

let main () = 
    print_int (iter (fun x -> x * x) 2 4);
    print_char '\n' ;
    print_int (iter (fun x -> x * 2) 2 4);
    print_char '\n' ;
    print_int (iter (fun x -> x + 1) 4 6);
    print_char '\n'

let () = main ()