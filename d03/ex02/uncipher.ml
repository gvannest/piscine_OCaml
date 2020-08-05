let unrot42 str =
    let unrot42_char c = match c with
        | 'a' .. 'z' -> begin char_of_int((int_of_char 'z') + ((int_of_char c) - (int_of_char 'a') - 42 + 1) mod 26) end
        | 'A' .. 'Z' -> begin char_of_int((int_of_char 'Z') + ((int_of_char c) - (int_of_char 'A') - 42 + 1) mod 26) end
        | _ -> c
    in
    String.map unrot42_char str

let uncaesar str ~arg1:n =
    let uncaesar_char c = match c with
        | 'a' .. 'z' -> begin 
            if ((int_of_char c) - (n mod 26)) >= (int_of_char 'a') then char_of_int (((int_of_char c) - (n mod 26)))
            else char_of_int (((int_of_char 'z') - ((int_of_char 'a') - ((int_of_char c) - (n mod 26)))) + 1)
        end
        | 'A' .. 'Z' -> begin
            if ((int_of_char c) - (n mod 26)) >= (int_of_char 'A') then char_of_int (((int_of_char c) - (n mod 26)))
            else char_of_int (((int_of_char 'Z') - ((int_of_char 'A') - ((int_of_char c) - (n mod 26)))) + 1)
        end
        | _ -> c
    in
    String.map uncaesar_char str

let rec ft_uncrypt (str:string) (list_fct:(string->string) list) = match list_fct with
    | [] -> str
    | h :: t -> ft_uncrypt (h str) t