type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
    phosphate : phosphate ;
    deoxyribose : deoxyribose ;
    nucleobase : nucleobase
}

let generate_nucleotide c =
    {
        phosphate = "phosphate" ;
        deoxyribose = "deoxyribose" ;
        nucleobase = match c with
                    | 'A' -> A
                    | 'T' -> T
                    | 'C' -> C
                    | 'G' -> G
                    | _ -> None
    }
