let ft_test_sign int_to_test =
    if int_to_test < 0
    then "negative"
    else "positive"

let main () =
    print_endline (ft_test_sign 2);
    print_endline (ft_test_sign 0);
    print_endline (ft_test_sign (-42));
    print_endline (ft_test_sign 3000000000);
    print_endline (ft_test_sign (-3000000000))

let () = main ()