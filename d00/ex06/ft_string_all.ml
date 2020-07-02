
let ft_string_all func str =
    let len = String.length str in
    let rec apply current_idx =
        if current_idx == len
        then true
        else begin
            let current_char = String.get str current_idx in
            if not(func current_char)
            then false
            else apply (current_idx + 1)
        end
    in
    apply 0

let is_digit c = c >= '0' && c <= '9'

let main () =
    let a = ft_string_all is_digit "0123456789" in
    let b = ft_string_all is_digit "0123e56789" in
    if a
    then print_endline "true"
    else print_endline "false" ;
    if b
    then print_endline "true"
    else print_endline "false"


let () = main ()