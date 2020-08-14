
let rdString () =
    Random.self_init () ;
    let len = Random.int 12 in
    let rec loop (i:int) (res:string) = match i with
        | x when x = len -> res
        | _ -> loop (i + 1) (res ^ Dalek.charGenerator ())
    in loop 0 ""


let () =
    Random.self_init () ;
    let rec createListPeople (i:int) (res:People.people list) = match i with
        | 0 -> res
        | _ -> createListPeople (i - 1) ((new People.people (rdString())) :: res)
    in
    let rec createListDoctors (i:int) (peopleList:People.people list) (res:Doctor.doctor list) = match i with
        | 0 -> res
        | _ -> createListDoctors (i - 1) peopleList ((new Doctor.doctor (rdString ()) (Random.int 100) (List.nth peopleList (Random.int (List.length peopleList)))) :: res)
    in
    let rec createListDaleks (i:int) res = match i with
        | 0 -> res
        | _ -> createListDaleks (i - 1) ((new Dalek.dalek) :: res)
    in
    let lstPeople = createListPeople ((Random.int 15) + 3) [] in
    let lstDoctors = createListDoctors ((Random.int 15) + 3) lstPeople [] in
    let lstDaleks = createListDaleks ((Random.int 15) + 3) [] in
    let armyPeople = new Army.army lstPeople in
    let armyDoctors = new Army.army lstDoctors in
    let armyDaleks = new Army.army lstDaleks in
    let g = new Galifrey.galifrey armyDaleks armyDoctors armyPeople in
    g#do_time_war
