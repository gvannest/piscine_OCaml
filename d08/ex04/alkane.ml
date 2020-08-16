let getAlkaneName (n:int) = match n with
    | 1 -> "Methane"
    | 2 -> "Ethane"
    | 3 -> "Propane"
    | 4 -> "Butane"
    | 5 -> "Pentane"
    | 6 -> "Hexane"
    | 7 -> "Heptane"
    | 8 -> "Octane"
    | 9 -> "Nonane"
    | 10 -> "Decane"
    | 11 -> "Undecane"
    | 12 -> "Dodecane"
    | _ -> failwith "Error: not required to test with n > 12"

class virtual alkane (n:int) =
object (this)
    inherit Molecule.molecule (getAlkaneName n) ((List.init n (fun x -> new Atom.carbon)) @ (List.init (2 * n + 2) (fun x -> new Atom.hydrogen)))
end

class methane =
object
    inherit alkane 1
end

class ethane =
object
    inherit alkane 2
end

class propane =
object
    inherit alkane 3
end

class octane =
object
    inherit alkane 8
end