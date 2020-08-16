let () =
    let h = new Atom.hydrogen in
    let c = new Atom.carbon in
    let o = new Atom.oxygen in
    print_endline (h#to_string) ;
    print_endline (c#to_string) ;
    print_endline (o#to_string) ;
    let h2 = new Atom.hydrogen in
    print_endline (string_of_bool (h#equals h2)) ;
    print_endline (string_of_bool (h#equals c)) ;
    print_endline (string_of_bool (h#equals o)) 