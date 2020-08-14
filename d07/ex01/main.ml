let () =
    let sidekick_people = new People.people "Roger" in
    let doctor = new Doctor.doctor "Who" 42 sidekick_people in
    print_endline doctor#to_string ;
    doctor#talk ; print_char '\n' ;
    doctor#travel_in_time 0 (-10) ;
    print_endline doctor#to_string ;
    doctor#use_sonic_screwdriver ;  print_char '\n' ;
    doctor#setHp 50 ;
    print_endline doctor#to_string;
    (* doctor#regenerate ; *)
    doctor#test_regenerate ;
    print_endline doctor#to_string
