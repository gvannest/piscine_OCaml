let print_example (example:(float array * string)) : unit =
    print_string "([|" ;
    let rec print_array (l:float list) = match l with
        | h :: n :: t -> Printf.printf "%f; " h ; print_array (n :: t)
        | h :: t -> Printf.printf "%f|]" h
        | _ -> ()
    in
    print_array (Array.to_list (fst example)) ;
    Printf.printf ", %s)\n" (snd example)

let rec print_serie (serie:(float array * string) list) = match serie with
    | [] -> print_char '\n'
    | h :: t -> print_example h ; print_serie t

let examples_of_file (filename:string) : (float array * string) list =
    let file = open_in filename in 
    let result = ref [] in
    begin try
    while true do
        let example = input_line file in
        let listOfStrings = String.split_on_char ',' example in
        let listRev = List.rev listOfStrings in
        let arrayFloats = (Array.of_list (List.map float_of_string (List.rev (List.tl listRev)))) in
        let pair = (arrayFloats, List.hd listRev) in
        result := pair :: !result
    done ; !result
    with
    | Sys_error err -> close_in file ; Printf.printf "Something wrong happened : %s\n" err ; []
    | End_of_file -> close_in file ; List.rev !result
    end 


let main (argc:int) (argv:string array) = match argc with 
    | x when x <> 2 -> Printf.printf "Usage: ./a.out filename"
    | _ -> print_serie (examples_of_file (argv.(1)))


let () =
    let argv = Sys.argv in
    main (Array.length argv) argv



