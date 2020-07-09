
let crossover lst1 lst2 =
    if lst1 = [] || lst2 = [] then []
    else begin
        let rec crossover_1 res liste_1 =
            match liste_1 with
                | [] -> res
                | head1 :: tail1 -> let rec crossover_2 res_appended liste_2 =
                    match liste_2 with
                        | [] -> crossover_1 res_appended tail1
                        | head2 :: tail2 when head2 <> head1 -> crossover_2 res_appended tail2
                        | head2 :: tail2 -> crossover_2 (res_appended@[head2]) tail2
                    in
                    crossover_2 res lst2
        in
        crossover_1 [] lst1
    end



let rec print_result_int = function
    | [] -> ()
    | e :: l -> print_int e ; print_string ", " ; print_result_int l


let rec print_result_str = function
    | [] -> ()
    | e :: l -> print_string e ; print_string ", " ; print_result_str l


let main () =
    let a = [12; 3; 0; 7; -3; 9; -1] in
    let b = [4; -2; 9; 13; 12; -3; 11] in
    let c = [] in
    let x = ["toto"; "tata"; "titi"; "toutou"; "gotgot"; "kikou"; ""] in
    let y = ["tamtam"; "tutu"; "gotgot"; "toto"; "gregou"; "tata"; "liloulol"] in
    print_result_int (crossover a b) ;
    print_char '\n' ;
    print_result_int (crossover a c) ;
    print_char '\n' ;
    print_result_str (crossover x y) ;
    print_char '\n'


let () = main ()