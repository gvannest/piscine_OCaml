module type SET =
sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val foreach : 'a t -> ('a -> unit) -> unit
    val for_all : 'a t -> ('a -> bool) -> bool
    val exists : 'a t -> ('a -> bool) -> bool
    val append : 'a t -> 'a -> 'a t
    val concat : 'a t -> 'a t -> 'a t
    val print_set_int : int t -> unit
end

module Set : SET =
struct
    type 'a t = 'a list
    let return x = [x]
    let bind set f = List.concat (List.map f set)

    let union set_a set_b = 
        let combine_set = set_a @ set_b in
        let rec loop_unique set res = match set with
            | [] -> res
            | h :: t when (List.find_opt (fun x -> x = h) res) = None -> loop_unique t (res @ [h])
            | h :: t -> loop_unique t res
        in loop_unique combine_set []

    let inter set_a set_b = 
        List.filter (fun x -> let is_in_setb = List.find_opt (fun a -> a = x) set_b in if is_in_setb = None then false else true) set_a

    let diff set_a set_b = 
        let a_not_b = List.filter (fun x -> let is_not_in_setb = List.find_opt (fun a -> a = x) set_b in if is_not_in_setb = None then true else false) set_a in
        let b_not_a = List.filter (fun x -> let is_not_in_seta = List.find_opt (fun a -> a = x) set_a in if is_not_in_seta = None then true else false) set_b in
        a_not_b @ b_not_a

    let filter set p = List.filter p set
    let foreach set f = List.iter f set
    let for_all set p = List.for_all p set
    let exists set p = List.exists p set
    let append set a = if (List.find_opt (fun x -> x = a) set) = None then (set @ [a]) else set
    let concat set1 set2 = union set1 set2

    let rec print_set_int set = match set with
        | [] -> print_char '\n'
        | h :: t -> print_string ((string_of_int h) ^ " ") ; print_set_int t

end


let () =
    let set = Set.concat (Set.concat (Set.concat (Set.return 10) (Set.return 5)) (Set.return 8)) (Set.return (-5)) in
    let set2 = Set.concat (Set.concat (Set.concat (Set.return 2) (Set.return 10)) (Set.return 0)) (Set.return (-1)) in
    let set3 = Set.concat (Set.concat (Set.concat (Set.return 13) (Set.return 5)) (Set.return (-5))) (Set.return 3) in
    Set.print_set_int set; 
    Set.print_set_int set2; 
    Set.print_set_int set3;
    Set.print_set_int (Set.union (Set.union set set2) set3); 
    Set.print_set_int (Set.inter set set3);
    Set.print_set_int (Set.diff set set3);
    Set.print_set_int (Set.filter set2 (fun x -> x > 0));
    Set.foreach set (fun x -> print_string ((string_of_int (x * x)) ^ " "));
    print_char '\n' ;
    print_endline (string_of_bool (Set.for_all set3 (fun x -> x mod 2 <> 0)));
    print_endline (string_of_bool (Set.for_all set (fun x -> x mod 2 <> 0)));
    print_endline (string_of_bool (Set.exists set (fun x -> x mod 2 <> 0)))