let () =
    let gotgot = new People.people "Gautier" in
    print_endline gotgot#to_string ;
    gotgot#talk ;
    print_char '\n' ;
    gotgot#die