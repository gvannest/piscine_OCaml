let rec ft_print_string_of_list = function
    | [] -> print_char '\n'
    | head :: tail -> print_char head ; ft_print_string_of_list tail

let sequence n =
    if n < 1 then ft_print_string_of_list []
    else begin
        let rec new_list lst_in lst_out count = match lst_in with
            | [] -> lst_out
            | head :: next :: tail when head = next -> new_list (next :: tail) lst_out (count + 1)
            | head :: tail -> new_list tail (lst_out@[char_of_int (int_of_char '0' + count + 1); head]) 0
        in
        let rec sequence_aux x res = match x with
            | y when y = n -> ft_print_string_of_list res
            | _ -> sequence_aux (x + 1) (new_list res [] 0)
        in
        sequence_aux 1 ['1']
    end


let () =
    sequence 0;
    sequence 1;
    sequence 2;
    sequence 3;
    sequence 4;
    sequence 5;
    sequence 6;
    sequence 7
