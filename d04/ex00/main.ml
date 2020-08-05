

let main () =
    let rec printAll l = match l with
        | [] -> print_char '\n'
        | h :: t -> begin print_endline (Color.toString h) ; print_endline (Color.toStringVerbose h) end ; printAll t

    in
    printAll Color.all

let () = main ()    