type 'a ft_ref = {mutable contents : 'a}

let return (x:'a) = {contents = x}

let get (x:'a ft_ref) = x.contents

let set (x:'a ft_ref) (y:'a) = x.contents <- y

let bind (x:'a ft_ref) (f:('a -> 'b ft_ref)) = f (get x)


let () =
    let my_ref = return 42 in
    print_endline (string_of_int (get my_ref)) ;
    ignore (set my_ref 9) ;
    print_endline (string_of_int (get my_ref)) ;
    let my_transformed_ref = bind my_ref (fun x -> return (string_of_int (x * x) ^ " Huh?!")) in
    print_endline (get my_transformed_ref) ;
    ignore (set my_transformed_ref "coucou") ;
    print_endline (get my_transformed_ref)


