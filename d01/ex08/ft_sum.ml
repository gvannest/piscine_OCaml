
let ft_sum f lower upper =
    if upper < lower then nan
    else begin
        let rec ft_sum_aux current_idx acc =
            if current_idx = upper then ((f current_idx) +. acc)
            else ft_sum_aux (current_idx + 1) ((f current_idx) +. acc)
        in
        ft_sum_aux lower 0.0
    end


let main () =
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10) ;
    print_char '\n' ;
    print_float (ft_sum (fun i -> float_of_int ((i + 1) * (i + 1) * (i + 1))) 0 4) ;
    print_char '\n'


let () = main ()