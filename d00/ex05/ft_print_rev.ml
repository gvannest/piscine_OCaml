
let ft_print_rev str =
    let start = (String.length str) - 1 in
    let rec loop current_idx =
        if current_idx >= 0
        then begin
            print_char (String.get str current_idx) ;
            loop (current_idx - 1)
        end
    in
    loop start ;
    print_char '\n'


let main () =
    ft_print_rev "Hello World !" ;
    ft_print_rev "" ;
    ft_print_rev "0123456789"

let () = main ()