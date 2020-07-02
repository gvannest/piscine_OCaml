
let rec ackermann m n =
    if m = (-1) || n = (-1)
    then (-1)
    else if m = 0 then (n + 1)
    else if n = 0 then ackermann (m - 1) 1
    else ackermann (m - 1) (ackermann m (n - 1))


let main () =
    print_int (ackermann (-1) 7) ;
    print_char '\n' ;
    print_int (ackermann 0 0) ;
    print_char '\n' ;
    print_int (ackermann 2 3) ;
    print_char '\n'

let () = main()