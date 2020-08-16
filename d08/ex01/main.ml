let () =
    let w = new Molecule.water in
    let cd = new Molecule.carbon_dioxyde in
    let m = new Molecule.methane in
    let no = new Molecule.nitrous_oxyde in
    let g = new Molecule.glucose in
    let alo = new Molecule.aluminium_oxyde in
    print_endline (w#to_string) ;
    print_endline (cd#to_string) ;
    print_endline (m#to_string) ;
    print_endline (no#to_string) ;
    print_endline (g#to_string) ;
    print_endline (alo#to_string) ;
    let m2 = new Molecule.methane in
    print_endline (string_of_bool (w#equals m)) ;
    print_endline (string_of_bool (m#equals m2))