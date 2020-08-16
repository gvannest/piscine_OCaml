module type WATCHTOWER = sig
    type hour = int
    val zero : hour
    val add : hour -> hour -> hour
    val sub : hour -> hour -> hour
end

module Watchtower : WATCHTOWER = struct
    type hour = int
    let zero = 12
    let add h1 h2 = (h1 + (h2 mod zero)) mod zero
    let sub h1 h2 = let factor = if h1 < (h2 mod zero) then 12 else 0 in factor + ((h1 - (h2 mod zero)) mod zero) 
end

let () =
    let h1:Watchtower.hour = 6 in
    let h2:Watchtower.hour = 8 in
    let h1_plus_h2 = Watchtower.add h1 h2 in
    let h1_sub_h2 = Watchtower.sub h1 h2 in
    print_endline (string_of_int h1_plus_h2) ;
    print_endline (string_of_int h1_sub_h2) ;
    let h3:Watchtower.hour = 25 in
    let h1_plus_h3 = Watchtower.add h1 h3 in
    print_endline (string_of_int h1_plus_h3) ;
    print_endline (string_of_int (Watchtower.sub 8 4));
    print_endline (string_of_int (Watchtower.sub 3 4));
    print_endline (string_of_int (Watchtower.sub 3 26))