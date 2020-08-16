let generate_formula (atoms:Atom.atom list) : string =
    let rec getOccurences (lst:Atom.atom list) (atom:Atom.atom) (n:int) = match lst with
        | [] -> n
        | h :: t when atom#equals h -> getOccurences t atom (n + 1)
        | h :: t -> getOccurences t atom n
    in
    let rec inter_list (lst:Atom.atom list) (uniques:Atom.atom list) (res:(string * int) list) = match lst with
        | [] -> res
        | h :: t when (List.exists (fun x -> h#equals x) uniques) -> inter_list t uniques res
        | h :: t -> inter_list t (h :: uniques) ((h#getSymbol, getOccurences atoms h 0) :: res)
    in
    let rec split_list (lst:(string * int) list) (ch:(string * int) list) (other:(string * int) list) (c:int) = match lst with
        | [] -> begin 
            if c = 0 then List.sort (fun x y -> String.compare (fst x) (fst y)) (ch @ other)
            else ch @ (List.sort (fun x y -> String.compare (fst x) (fst y)) other)
        end
        | h :: t when fst h = "C" -> split_list t (h :: ch) other 1
        | h :: t when fst h = "H" -> split_list t (ch @ [h]) other c
        | h :: t -> split_list t ch (h :: other) c
    in
    let to_string (fullLst:(string * int) list) =
        let pair_to_string (x, y) = if y <> 1 then (x ^ (string_of_int y)) else x in
        let rec loop (lst:(string * int) list) (res:string) = match lst with
            | [] -> res
            | h :: t -> loop t (res ^ (pair_to_string h))
        in loop fullLst ""
    in
    let intermediate_list = inter_list atoms [] [] in
    let sorted_list = split_list intermediate_list [] [] 0 in
    to_string sorted_list


class virtual molecule (name:string) (atoms: Atom.atom list) =
object (this)
    method name : string = name
    method formula : string = generate_formula atoms

    method to_string = "Molecule's name : " ^ this#name ^ ", molecule's formula : " ^ this#formula
    method equals (that:molecule) = (this#name = that#name) && (this#formula = that#formula)
    method getAtoms : (Atom.atom list) = atoms
end

class water =
object
    inherit molecule "Water" [new Atom.hydrogen; new Atom.oxygen; new Atom.hydrogen]
end

class carbon_dioxyde =
object
    inherit molecule "Carbon dioxyde" [new Atom.oxygen; new Atom.oxygen; new Atom.carbon]
end

class methane =
object
    inherit molecule "Methane" [new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.carbon]
end

class dioxygen =
object
    inherit molecule "dioxygen" [new Atom.oxygen; new Atom.oxygen]
end

class nitrous_oxyde =
object
    inherit molecule "Nitrous oxyde" [new Atom.nitrogen; new Atom.oxygen; new Atom.nitrogen]
end

class glucose =
object
    inherit molecule "Glucose" ((List.init 6 (fun x -> new Atom.oxygen)) @ (List.init 12 (fun x -> new Atom.hydrogen)) @ (List.init 6 (fun x -> new Atom.carbon)))
end

class  aluminium_oxyde =
object
  inherit molecule "Aluminium oxyde" [new Atom.oxygen ; new Atom.oxygen ; new Atom.aluminium ; new Atom.oxygen ; new Atom.aluminium]
end