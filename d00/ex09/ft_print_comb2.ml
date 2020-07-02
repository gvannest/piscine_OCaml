
let print_numbers a b =
    if (a / 10 == 0)
    then begin print_char '0' ; print_int a end
    else print_int a ;
    print_char ' ' ;
    if (b / 10 == 0)
    then begin print_char '0' ; print_int b end
    else print_int b ;
    if not(a = 98 && b = 99)
    then begin print_char ',' ; print_char ' ' end


let ft_print_comb2 () =
    let rec loop a b =
        if a <= 98
        then
            if b <= 99 && b > a
            then
                print_numbers a b ;
            if b < 99
            then loop a (b + 1)
            else if b == 99
            then loop (a + 1) (a + 2)
    in
    loop 0 1 ;
    print_char '\n'

let main () =
    ft_print_comb2 ()

let () = main ()