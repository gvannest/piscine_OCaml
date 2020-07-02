
let ft_is_palindrome str =
    let first = 0 in
    let last = String.length str - 1 in
    let rec loop first_idx last_idx =
        if last < 0 || first_idx >= last_idx
        then true
        else
            let first_char = String.get str first_idx in
            let last_char = String.get str last_idx in
            if first_char <> last_char
            then false
            else loop (first_idx + 1) (last_idx - 1)
    in
    loop first last


let main () =
    let a = "madam" in
    let b = "radar" in
    let c = "" in
    let d = "abcde" in
    print_endline "-----------" ;
    print_endline a ;
    if ft_is_palindrome a
    then print_endline "true"
    else print_endline "false" ;
    print_endline "-----------" ;
    print_endline b ;
    if ft_is_palindrome b
    then print_endline "true"
    else print_endline "false" ;
    print_endline "-----------" ;
    print_endline c ;
    if ft_is_palindrome c
    then print_endline "true"
    else print_endline "false" ;
    print_endline "-----------" ;
    print_endline d ;
    if ft_is_palindrome d
    then print_endline "true"
    else print_endline "false"

let () = main ()