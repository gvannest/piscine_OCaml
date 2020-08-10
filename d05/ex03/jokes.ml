let selectJoke (a:string array) (len:int) =
    Random.self_init () ;
    let idx = Random.int len in
    a.(idx)


let main (argc:int) (argv:string array) = match argc with
    | x when x <> 2 -> print_endline "Usage: ./a.out name_of_file"
    | _ -> begin
        let filename = argv.(1) in
        let channel = open_in filename in
        let arrayLen = ref 0 in
        try
            while true do
                ignore (input_line channel) ; incr arrayLen
                (* let a = Array.make 1 "" in let result = Array.append result a in
                a.(0) <- input_line channel ; ignore (result) *)
            done
        with
        End_of_file -> begin
            seek_in channel 0 ;
            let a = Array.make !arrayLen "" in
            try
                for i = 0 to (!arrayLen - 1) do
                    let str = input_line channel in
                    a.(i) <- str
                done ;
                print_endline (selectJoke a !arrayLen)
            with
            | Sys_error err -> Printf.printf "Error: Something went wrong : %s\n" err
            | End_of_file -> Printf.printf "Error: reached end of file\n"
        end ;
        close_in channel
    end

let () = 
    let argv = Sys.argv in
    main (Array.length argv) argv