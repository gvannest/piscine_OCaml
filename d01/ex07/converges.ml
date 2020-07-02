let rec converges f x n =
    if n == 0
    then begin
        if (f x) == x then true
        else false
    end
    else converges f (f x) (n - 1)

let main () = 
    if converges (fun x -> x * 2) 2 4 == true then print_endline "true" else print_endline "false" ;
    if converges (fun x -> x + 1) 4 6 == true then print_endline "true" else print_endline "false" ;
    if converges (fun x -> x / 2) 2 3 == true then print_endline "true" else print_endline "false" ;
    if converges (fun x -> x / 2) 2 2 == true then print_endline "true" else print_endline "false" ;
    if converges (( * ) 2) 2 5 == true then print_endline "true" else print_endline "false"

let () = main ()