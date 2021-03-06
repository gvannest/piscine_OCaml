type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
    phosphate : phosphate ;
    deoxyribose : deoxyribose ;
    nucleobase : nucleobase
}

type helix = nucleotide list

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

let generate_helix n =
    if n < 1 then begin print_endline "Error: number of nucleotides must be greater than 0 to generate an helix" ; [] end
    else begin
        Random.self_init() ;
        let rec choice = function
            | 0 -> 'A' 
            | 1 -> 'T' 
            | 2 -> 'C' 
            | 3 -> 'G'
            | _ -> 'O'
        in
        let rec gen_helix_aux i (acc: helix) = match i with
            | y when y = n -> acc
            | _ -> gen_helix_aux (i + 1) ((generate_nucleotide (choice (Random.int 4))) :: acc)
        in
        gen_helix_aux 0 []
    end


type strOption = String of string | None
let extractStrOption value = match value with
    | None -> "N/A"
    | String str -> str


let helix_to_string (lst: helix) =
    let nucleobase_str = function
        | A -> String "A"
        | T -> String "T"
        | C -> String "C"
        | G -> String "G"
        | _ -> None
    in
    let rec loop lst_remaining str = match lst_remaining with
        | [] -> str
        | h :: t -> begin
            let nclbase = nucleobase_str h.nucleobase in
            match nclbase with
                | None -> loop t str
                | _ -> loop t (str ^ (extractStrOption nclbase))
        end
    in
    loop lst ""


let complementary_helix (helix:helix) = 
    let get_complement = function
        | A -> 'T'
        | T -> 'A'
        | C -> 'G'
        | G -> 'C'
        | _ -> 'O'
    in
    let rec loop remaining_helix (comp_helix:helix) = match remaining_helix with
            | [] -> comp_helix
            | h :: t -> loop t (comp_helix@[generate_nucleotide (get_complement h.nucleobase)])
    in
    loop helix []


let () =
    let hlx = generate_helix 5 in print_endline (helix_to_string hlx) ; print_endline (helix_to_string (complementary_helix hlx)) ;
    print_endline "------------" ;
    let hlx1 = generate_helix 6 in print_endline (helix_to_string hlx1) ; print_endline (helix_to_string (complementary_helix hlx1)) ;
    print_endline "------------" ;
    let hlx2 = generate_helix 7 in print_endline (helix_to_string hlx2) ; print_endline (helix_to_string (complementary_helix hlx2)) ;
    print_endline "------------" ;
    let hlx3 = generate_helix 8 in print_endline (helix_to_string hlx3) ; print_endline (helix_to_string (complementary_helix hlx3)) ;
    print_endline "------------" ;
    let hlx4 = generate_helix 0 in print_endline (helix_to_string hlx4)