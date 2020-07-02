let ft_print_alphabet () =
    let first_letter = int_of_char 'a' in
    let last_letter = int_of_char 'z' in
    let rec loop current_letter =
        if current_letter <= last_letter then
        begin
            print_char (char_of_int current_letter);
            loop (current_letter + 1)
        end
    in 
    loop first_letter;
    print_char '\n'

let main () =
    ft_print_alphabet ()

let () = main ()