module type TRY =
  sig
    type 'a t = Success of 'a | Failure of exn
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val recover : 'a t -> (exn -> 'a t) -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val flatten : 'a t t -> 'a t
  end

module Try : TRY = struct
    type 'a t = Success of 'a | Failure of exn
    let return x = Success x
    let bind x f = match x with
        | Success n -> f n
        | Failure e -> Failure e

    let recover x ferr = match x with
        | Success n -> x
        | Failure e -> ferr e

    let filter x p = match x with
        | Success n when (p n) = false -> Failure (Failure "Monad does not satisfy predicate")
        | Success n -> x 
        | Failure e -> Failure e

    let flatten x = match x with
        | Success n -> n
        | Failure err -> Failure err
end

let () = 
    let string_of_int_try t = match t with
        | Try.Success suc -> "Success {" ^ string_of_int suc ^ "}"
        | Try.Failure (Invalid_argument mess) -> mess
        | _ -> "Unknown failure" in

    let div a b = if b = 0
        then Try.Failure (Invalid_argument "are you trying to divide by zero?")
        else Try.return (a / b) in

    print_endline "\n===testing Success and return===" ;
    let try_ok = div 42 2 in print_endline (string_of_int_try try_ok) ;

    print_endline "\n===testing failure===" ;
    let try_ko = div 42 0 in print_endline (string_of_int_try try_ko) ;

    print_endline "\n===testing bind===" ;
    print_endline (string_of_int_try (Try.bind try_ok (div 142))) ;
    print_endline (string_of_int_try (Try.bind try_ko (div 142))) ;
    let to_string a = if a = 0 then Try.Failure (Invalid_argument "je n'imprimerai pas 0") else Try.return (string_of_int a) in
    let string_of_str_try t = match t with
        | Try.Success suc -> "Success {" ^ suc ^ "}"
        | Try.Failure (Invalid_argument mess) -> mess
        | _ -> "Unknown failure" in
    print_endline (string_of_str_try (Try.bind (Try.return 0) to_string)) ;
    print_endline (string_of_str_try (Try.bind (Try.return 42) to_string)) ;

    print_endline "\n===testing recover===" ;
    let set_zero ex = Try.Success 0 in
    print_endline (string_of_int_try (Try.recover try_ko set_zero)) ;
    let is_divisible_by d i = i mod d = 0 in
    print_endline (string_of_int_try (Try.recover try_ko set_zero)) ;

    print_endline "\n===testing filter===" ;
    print_endline (string_of_int_try (Try.filter try_ok (is_divisible_by 2))) ;
    print_endline (string_of_int_try (Try.filter try_ok (is_divisible_by 3))) ;

    print_endline "\n===testing flatten===" ;
    let meta_ok = Try.Success try_ok in print_endline (string_of_int_try (Try.flatten meta_ok)) ;
    let meta_ko = Try.Success try_ko in print_endline (string_of_int_try (Try.flatten meta_ko)) ;