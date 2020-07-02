let rec ft_power a b = 
    if a == 0
    then 0
    else if b == 0
    then 1
    else if b == 1
    then a
    else a * (ft_power a (b - 1))

let main () =
    print_int (ft_power 2 3);
    print_char '\n';
    print_int (ft_power 2 0);
    print_char '\n';
    print_int (ft_power 0 3);
    print_char '\n'

let () = main ()