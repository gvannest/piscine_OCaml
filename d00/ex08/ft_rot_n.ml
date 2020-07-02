
let ft_rot_n n str =
    let rot = n mod 26 in
    let is_alpha_small c = c >= 'a' && c <= 'z' in
    let is_alpha_cap c =  c >= 'A' && c <= 'Z' in
    let ft_rot current_char =
        if is_alpha_small current_char
        then char_of_int ((int_of_char 'a') + ((int_of_char current_char) + rot - (int_of_char 'a')) mod 26)
        else if is_alpha_cap current_char
        then char_of_int ((int_of_char 'A') + ((int_of_char current_char) + rot - (int_of_char 'A')) mod 26)
        else current_char
    in
    String.map ft_rot str


let main () =
    print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz") ;
    print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz") ;
    print_endline (ft_rot_n 42 "0123456789") ;
    print_endline (ft_rot_n 2 "OI2EAS67B9") ;
    print_endline (ft_rot_n 0 "Damned !") ;
    print_endline (ft_rot_n 42 "") ;
    print_endline (ft_rot_n 1 "NBzlk qnbjr !")


let () = main ()