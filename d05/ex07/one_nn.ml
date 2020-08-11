type radar = float array * string

let eu_dist (a:float array) (b:float array) : float =
    let diff (x:float) (y:float) = x -. y
    in
    let square (x:float) = x *. x
    in
    let sum (x:float) (y:float) = x +. y
    in
    let toSum = Array.map square (Array.map2 diff a b) in
    let toSroot = Array.fold_left sum 0.0 toSum in
    sqrt toSroot

let min a b = if a <= b then a else b

let one_nn (examples:radar list) (radar:radar) : string =
    let rec computeDistances (l:(float array) list) (result:float list) : float list = match l with
        | [] -> List.rev result
        | h :: t -> computeDistances t ((eu_dist (fst radar) h) :: result)
    in
    let distanceList = computeDistances (fst (List.split examples)) []
    in
    let minDist = List.fold_left min (List.hd distanceList) distanceList
    in
    let rec findIdx (l:float list) (i:int) = match l with
        | [] -> failwith "Error : minimum not found"
        | h :: t when h = minDist -> i 
        | h :: t -> findIdx t (i + 1)
    in
    let minIdx = findIdx distanceList 0 in
    snd (List.nth examples minIdx)


let main (argc:int) (argv:string array) = match argc with
    | x when x <> 2 -> Printf.printf "Usage: ./a.out filename"
    | _ -> begin
        let test = ([|1.;0.;0.50932;-0.93996;1.;0.26708;-0.03520;-1.;1.;-1.;0.43685;-1.;0.;0.;-1.;-0.34265;-0.37681;0.03627;1.;-1.;0.;0.;0.;0.;-0.16253;0.92236;0.39752;0.26501;0.;0.;1.;0.23188;0.;0.|], "c") in (* b *)
        let test2 = ([|1.;0.;0.96071;0.07088;0.;0.04296;1.;0.09313;0.90169;-0.05144;0.89263;0.02580;0.83250;-0.06142;0.87534;0.09831;0.76544;0.00280;0.75206;-0.05295;0.65961;-0.07905;0.64158;-0.05929;0.55677;-0.07705;0.58051;-0.02205;0.49664;-0.01251;0.51310;-0.00015;0.52099;-0.00182|], "c") in (* g *)
        let trainingSet = Examples_of_file.examples_of_file argv.(1) in
        print_endline (one_nn trainingSet test) ;
        print_endline (one_nn trainingSet test2)

    end


let () =
    let argv = Sys.argv in
    main (Array.length argv) argv

    



