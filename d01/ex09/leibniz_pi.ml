let leibniz_pi delta =
    let pi = 4. *. (atan 1.) in
    let absolute x = if x >= 0. then x else (-.x) in
    if delta < 0. then (-1)
    else begin
        let rec pi_aux iter pi_calc =
            if absolute (pi -. pi_calc) <= delta then iter
            else pi_aux (iter + 1) (pi_calc +. (4. *. ((-1.) ** (float_of_int iter)) /. (2. *. (float_of_int iter) +. 1.)))
        in
        pi_aux 0 0.
    end


let main () =
    print_int (leibniz_pi 1.) ;
    print_char '\n' ;
    print_int (leibniz_pi 0.5) ;
    print_char '\n' ;
    print_int (leibniz_pi 0.3) ;
    print_char '\n' ;
    print_int (leibniz_pi 0.01) ;
    print_char '\n' ;
    print_int (leibniz_pi (-.0.2)) ;
    print_char '\n'

let () = main ()