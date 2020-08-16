exception Unbalanced of string

let rec getOccurences (lst:'a list) (atom:'a) (n:int) = match lst with
    | [] -> n
    | h :: t when atom#equals h -> getOccurences t atom (n + 1)
    | h :: t -> getOccurences t atom n

let count_atoms (pair_mol_count:(Molecule.molecule * int) list) : (string * int) list  =
    let rec list_atoms_occur (fullLst:Atom.atom list) (lst:Atom.atom list) (uniques:Atom.atom list) (res:(string * int) list) = match lst with
        | [] -> res
        | h :: t when (List.exists (fun x -> h#equals x) uniques) -> list_atoms_occur fullLst t uniques res
        | h :: t -> list_atoms_occur fullLst t (h :: uniques) ((h#getSymbol, getOccurences fullLst h 0) :: res)
    in
    let rec flat_loop (lst: Atom.atom list) (n:int) (res:Atom.atom list) = match n with
        | i when i = 0 -> res
        | _ -> flat_loop lst (n - 1) (res @ lst)
    in
    let break_mol = List.map (fun (mol, count) -> (mol#getAtoms, count)) pair_mol_count in
    let flattened = List.concat (List.map (fun (l, n) -> flat_loop l n []) break_mol) in
    list_atoms_occur flattened flattened [] []

let compare_list_atoms (react:(string * int) list) (prod:(string * int) list) =
    let rec loop lst_react = match lst_react with
        | [] -> true
        | (symbol, count) :: t -> begin 
            let count_in_prod = List.assoc_opt symbol prod in
            if count_in_prod = None then false
            else if Option.get count_in_prod <> count then false
            else loop t
        end
    in
    loop react

let mol_occur_list (molecules:Molecule.molecule list) : (Molecule.molecule * int) list =
    let rec loop (lst:Molecule.molecule list) (uniques:Molecule.molecule list) (res:(Molecule.molecule * int) list) = match lst with
        | [] -> res
        | h :: t when (List.exists (fun x -> h#equals x) uniques) -> loop t uniques res
        | h :: t -> loop t (h :: uniques) ((h, getOccurences molecules h 0) :: res) 
    in loop molecules [] []

let rec add_molecule (lst:(Molecule.molecule * int) list) (mol:Molecule.molecule) res = match lst with
    | [] -> res
    | (h, count) :: t when (h#equals mol) -> add_molecule t mol (res @ [(h, count + 1)])
    | h :: t -> add_molecule t mol (res @ [h])

let count_indiv_atom (atom:Atom.atom) (molecule:Molecule.molecule) =
    getOccurences (molecule#getAtoms) atom 0

let getCoeff (reactives: (Molecule.molecule * int) list) (products: (Molecule.molecule * int) list) =
    let rec calc_coeff n =
        print_endline (string_of_int n) ;
        let p = (count_indiv_atom (new Atom.carbon) (fst (List.hd reactives))) * n in
        print_endline (string_of_int p) ;
        let q = ((count_indiv_atom (new Atom.hydrogen) (fst (List.hd reactives))) * n) / 2 in
        print_endline (string_of_int q) ;
        if q mod 2 = 0 then (n,(p + q / 2), p, q) else calc_coeff (2 * n)
    in calc_coeff (snd (List.hd reactives))
            


class alkane_combustion (alkanes: Alkane.alkane list) =
object (this)

    inherit Reaction.reaction ((alkanes :> Molecule.molecule list) @ [new Molecule.dioxygen]) ([new Molecule.water; new Molecule.carbon_dioxyde])

    val _reactives : (Molecule.molecule * int) list = mol_occur_list (List.rev ((alkanes :> Molecule.molecule list) @ [new Molecule.dioxygen]))
    val _products : (Molecule.molecule * int) list = mol_occur_list [new Molecule.water; new Molecule.carbon_dioxyde]

    method get_start : (Molecule.molecule * int) list = match this#is_balanced with
        | false -> raise (Unbalanced "Error : The reaction is not balanced")
        | true -> _reactives

    method get_result : (Molecule.molecule * int) list = match this#is_balanced with
        | false -> raise (Unbalanced "Error : The reaction is not balanced")
        | true -> _products

    method pair_react_prod : ((Molecule.molecule * int) list) * ((Molecule.molecule * int) list) =
        let (n, m, p, q) = getCoeff _reactives _products in
        (* print_endline (string_of_int n) ; print_endline (string_of_int m) ; print_endline (string_of_int p) ; print_endline (string_of_int q) ; *)
        let rec newReact lst_react res_react = match lst_react with
            | [] -> res_react
            | (mol, count) :: t when (mol#equals (new Molecule.dioxygen)) -> newReact t (res_react @ [(mol, m)])
            | (mol, count) :: t -> newReact t (res_react @ [(mol, n)])
        in
        let rec newProd lst_prod res_prod = match lst_prod with
            | [] -> res_prod
            | (mol, count) :: t when (mol#equals (new Molecule.carbon_dioxyde)) -> newProd t (res_prod @ [(mol, p)])
            | (mol, count) :: t -> newProd t (res_prod @ [(mol, q)])
        in
        (newReact _reactives [], newProd _products [])


    method balance : Reaction.reaction =
        let (r, p) = this#pair_react_prod in
        ({< _reactives = r ; _products = p >} :> Reaction.reaction)

    method is_balanced : bool = 
        let atoms_int_reactives : (string * int) list = count_atoms _reactives in
        let atoms_int_products : (string * int) list = count_atoms _products in
        compare_list_atoms atoms_int_reactives atoms_int_products

end