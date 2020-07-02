
let fibonacci n =
    if n < 0 then (-1)
    else if n = 0 then 0
    else if n = 1 then 1
    else begin
        let rec fibo_aux x n1 n2 =
            if x = n then (n1 + n2)
            else fibo_aux (x + 1) (n1 + n2) n1
        in
        fibo_aux 2 1 0
    end


let main () =
    print_int (fibonacci (-42)) ;
    print_char '\n' ;
    print_int (fibonacci 1) ;
    print_char '\n' ;
    print_int (fibonacci 3) ;
    print_char '\n' ;
    print_int (fibonacci 6) ;
    print_char '\n' ;
    print_int (fibonacci 7) ;
    print_char '\n' ;
    print_int (fibonacci 8) ;
    print_char '\n' 


let () = main ()