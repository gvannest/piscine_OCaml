let ft_print_comb () =
    let h_init = 0 in 
    let t_init = 1 in 
    let u_init = 2 in
    let ft_print_digits current_hundred current_ten current_unit =
        begin
        print_int current_hundred ; print_int current_ten ; print_int current_unit ;
        if (current_hundred <> 7 || current_ten <> 8 || current_unit <> 9)
        then print_string ", " 
        end
    in
    let rec loop current_hundred current_ten current_unit =
        if current_hundred <= 7
        then 
            if current_ten <= 8 && current_ten > current_hundred
            then
                if current_unit <= 9 && current_unit > current_ten
                then begin
                    ft_print_digits current_hundred current_ten current_unit ;
                    if current_unit < 9
                    then loop current_hundred current_ten (current_unit + 1)
                    else if current_ten < 8
                    then loop current_hundred (current_ten + 1) (current_ten + 2)
                    else loop (current_hundred + 1) (current_hundred + 2) (current_hundred + 3)
                end
    in 
    loop h_init t_init u_init ;
    print_string "\n"


let main () =
    ft_print_comb ()

let () = main ()