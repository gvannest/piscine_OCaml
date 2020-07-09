let gray n =
    if n < 1 then print_endline "Error"
    else begin
        let limit = int_of_float (2. ** float_of_int(n))
        in
        let rec print_output x loop_nbr =
            let q = x / 2 in
            let r = x mod 2 in
            match q with
                | 0 when (loop_nbr = n || n = 0) -> print_int r
                | 0 -> print_int 0 ; print_output x (loop_nbr + 1)
                | _ -> print_output q (loop_nbr + 1); print_int r 
        in
        let rec gray_aux decimal = match decimal with
            | y when y = limit -> print_char '\n'
            | _ -> begin print_output (decimal lxor (decimal lsr 1)) 1 ; print_char ' ' ; gray_aux (decimal + 1) end
        in
        gray_aux 0
    end


let main () =
    gray 0 ;
    gray 1 ;
    gray 2 ;
    gray 3 ;
    gray 4 ;
    gray 5

let () = main ()