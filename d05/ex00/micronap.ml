

let my_sleep () = Unix.sleep 1

let main (argc:int) (argv:string array) = match argc with
    | x when x = 2 -> begin
        let timer = try int_of_string argv.(1) with Failure err -> raise (Invalid_argument "Error : please specify an interger.") in
        for i = timer downto 0 do
            my_sleep ()
        done
    end
    | x when x < 2 -> raise (Invalid_argument "Error : Missing argument")
    | _ -> raise (Invalid_argument "Error : Too many arguments")



let () = 
    let argv = Sys.argv in
    try main (Array.length argv) argv with
        | Invalid_argument err -> print_endline err
        | _ -> ()


(* ocamlopt unix.cmxa micronap.ml *)