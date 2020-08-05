let rot42 str =
    let rot42_char c = match c with
        | 'a' .. 'z' ->  begin char_of_int((int_of_char 'a') + (((int_of_char c) - (int_of_char 'a') + 42) mod 26)) end
        | 'A' .. 'Z' -> begin char_of_int((int_of_char 'A') + (((int_of_char c) - (int_of_char 'A') + 42) mod 26)) end
        | _ -> c
    in
    String.map rot42_char str


let caesar n str = 
    let caesar_char c = match c with
        | 'a' .. 'z' ->  begin char_of_int((int_of_char 'a') + (((int_of_char c) - (int_of_char 'a') + n) mod 26)) end
        | 'A' .. 'Z' -> begin char_of_int((int_of_char 'A') + (((int_of_char c) - (int_of_char 'A') + n) mod 26)) end
        | _ -> c
    in
    String.map caesar_char str


let xor (key:int) (str:string) = match key > 0 with
    | true -> let xor_char c = char_of_int ((int_of_char c) lxor (key mod 256)) in String.map xor_char str
    | false -> str


let rec ft_crypt (str:string) (list_fct:(string->string) list) = match list_fct with
        | [] -> str
        | h :: t -> ft_crypt (h str) t



