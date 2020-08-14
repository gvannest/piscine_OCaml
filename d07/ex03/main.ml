
let () =
    let people1 = new People.people "Xoriman" in
    let people2 = new People.people "Heroes" in
    let doctor1 = new Doctor.doctor "Who" 42 people1 in
    let doctor2 = new Doctor.doctor "Me" 98 people2 in
    let dalek1 = new Dalek.dalek in
    let dalek2 = new Dalek.dalek in
    
    print_endline "--------- Army of people ---------------" ;
    let armyPeople = new Army.army [people1; people2] in
    print_int (armyPeople#armySize) ; print_char '\n';
    armyPeople#add (new People.people "Dorian") ;
    armyPeople#add (new People.people "Francis") ;
    print_int (armyPeople#armySize) ; print_char '\n';
    armyPeople#armyMembersString ;
    armyPeople#delete ;
    print_int (armyPeople#armySize) ; print_char '\n';
    armyPeople#armyMembersString ;

    print_endline "--------- Army of doctors ---------------" ;
    let armyDoctors = new Army.army [doctor1; doctor2] in
    print_int (armyDoctors#armySize) ; print_char '\n';
    armyDoctors#armyMembersString ;
    armyDoctors#delete ;
    armyDoctors#armyMembersString ;
    armyDoctors#delete ;
    armyDoctors#armyMembersString ;
    armyDoctors#delete ;
    armyDoctors#armyMembersString ;
    armyDoctors#add (new Doctor.doctor "You?" 11 people1) ;
    armyDoctors#armyMembersString ;

    print_endline "--------- Army of Daleks ---------------" ;
    let armyDaleks = new Army.army [dalek1; dalek2] in
    print_int (armyDaleks#armySize) ; print_char '\n';
    armyDaleks#add (new Dalek.dalek) ;
    print_int (armyDaleks#armySize) ; print_char '\n';
    armyDaleks#armyMembersString ;
    armyDaleks#delete ;
    print_int (armyDaleks#armySize) ; print_char '\n';
    armyDaleks#armyMembersString
