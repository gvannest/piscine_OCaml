let sum (x:float) (y:float) : float = x +. y

let () =
    print_float (sum 3.2 4.5) ;
    print_char '\n' ;
    print_float (sum 0.0 4.5) ;
    print_char '\n' ;
    print_float (sum (-.3.2) 4.5) ;
    print_char '\n' 