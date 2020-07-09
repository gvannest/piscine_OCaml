type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None

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
                    | 'U' -> U
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

let nucleobase_str = function
    | A -> String "A"
    | T -> String "T"
    | C -> String "C"
    | G -> String "G"
    | U -> String "U"
    | _ -> None

let helix_to_string (lst: helix) =
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



(* ----------------- ex06 ----------------- *)

type rna = nucleobase list

let generate_rna (hlx:helix) =
    let compl_helix = complementary_helix hlx in
    let rec loop input_compl_helix (rna:rna) = match input_compl_helix with
        | [] -> rna
        | h :: t when h.nucleobase = T -> loop t (rna@[U])
        | h :: t -> loop t (rna@[h.nucleobase])
    in loop compl_helix []

let rec print_rna (rna:rna) = match rna with
    | [] -> print_char '\n'
    | h :: t -> print_string (extractStrOption (nucleobase_str h)) ; print_rna t



(* ----------------- ex07 ----------------- *)


let generate_bases_triplets (rna:rna) =
    let rec create_triplets rna_lst output = match rna_lst with
        | h :: a :: b :: tail -> create_triplets tail (output @ [(h, a, b)])
        | _ -> output
    in create_triplets rna []

let print_list_triplets lst =
    print_char '[' ;
    let rec loop new_lst = match new_lst with
        | [] -> print_char ']' ; print_char '\n'
        | h :: t -> match h with
                    | (a, b, c) -> begin
                                    print_char '(' ; print_string (extractStrOption (nucleobase_str a)) ;
                                    print_string ", ";
                                    print_string (extractStrOption (nucleobase_str b)) ;
                                    print_string ", ";
                                    print_string (extractStrOption (nucleobase_str c)) ;
                                    print_string "), ";
                                    loop t
                                end
    in loop lst


type aminoacid = Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val | Stop

type protein = aminoacid list

let string_of_protein (protein:protein) =
    let string_of_aminoacid = function
        | Ala -> "Alanine"
        | Arg -> "Arginine"
        | Asn -> "Asparagine"
        | Asp -> "Aspatique"
        | Cys -> "Cysteine"
        | Gln -> "Glutamine"
        | Glu -> "Glutamique"
        | Gly -> "Glycine"
        | His -> "Histidine"
        | Ile -> "Isoleucine"
        | Leu -> "Leucine"
        | Lys -> "Lysine"
        | Met -> "Methionine"
        | Phe -> "Phenylalanine"
        | Pro -> "Proline"
        | Ser -> "Serine"
        | Thr -> "Threonine"
        | Trp -> "Tryptophane"
        | Tyr -> "Tyrosine"
        | Val -> "Valine"
        | Stop -> "Stop"
    in
    let rec loop protein_loop output = match protein_loop with
        | [] -> output
        | h :: t -> if t <> [] then begin loop t (output ^ (string_of_aminoacid h) ^ ", ") end else begin output ^ (string_of_aminoacid h) end
    in
    loop protein ""


let decode_arn (rna:rna) = 
    let triplets = generate_bases_triplets rna in
    let rec decode_aux rna_tripl_lst (protein:protein) = match rna_tripl_lst with
        | (U,A,A) :: (U,A,G) :: (U,G,A) :: tail -> (protein@[Stop])
        | (G,C,A) :: (G,C,C) :: (G,C,G) :: (G,C,U) :: tail -> decode_aux tail (protein@[Ala])
        | (A,G,A) :: (A,G,G) :: (C,G,A) :: (C,G,C) :: (C,G,G) :: (C,G,U) :: tail -> decode_aux tail (protein@[Arg])
        | (A,A,C) :: (A,A,U) :: tail -> decode_aux tail (protein@[Asn])
        | (G,A,C) :: (G,A,U) :: tail -> decode_aux tail (protein@[Asp])
        | (U,G,C) :: (U,G,U) :: tail -> decode_aux tail (protein@[Cys])
        | (C,A,A) :: (C,A,G) :: tail -> decode_aux tail (protein@[Gln])
        | (G,A,A) :: (G,A,G) :: tail -> decode_aux tail (protein@[Glu])
        | (G,G,A) :: (G,G,C) :: (G,G,G) :: (G,G,U) :: tail -> decode_aux tail (protein@[Gly])
        | (C,A,C) :: (C,A,U) :: tail -> decode_aux tail (protein@[His])
        | (A,U,A) :: (A,U,C) :: (A,U,U) :: tail -> decode_aux tail (protein@[Ile])
        | (C,U,A) :: (C,U,C) :: (C,U,G) :: (C,U,U) :: (U,U,A) :: (U,U,G) :: tail -> decode_aux tail (protein@[Leu])
        | (A,A,A) :: (A,A,G) :: tail -> decode_aux tail (protein@[Lys])
        | (A,U,G) :: tail -> decode_aux tail (protein@[Met])
        | (U,U,C) :: (U,U,U) :: tail -> decode_aux tail (protein@[Phe])
        | (C,C,C) :: (C,C,A) :: (C,C,G) :: (C,C,U) :: tail -> decode_aux tail (protein@[Pro])
        | (U,C,A) :: (U,C,C) :: (U,C,G) :: (U,C,U) :: (A,G,U) :: (A,G,C) :: tail -> decode_aux tail (protein@[Ser])
        | (A,C,A) :: (A,C,C) :: (A,C,G) :: (A,C,U) :: tail -> decode_aux tail (protein@[Thr])
        | (U,G,G) :: tail -> decode_aux tail (protein@[Trp])
        | (U,A,C) :: (U,A,U) :: tail -> decode_aux tail (protein@[Tyr])
        | (G,U,A) :: (G,U,C) :: (G,U,G) :: (G,U,U) :: tail -> decode_aux tail (protein@[Val])
        | h :: tail -> decode_aux tail protein
        | [] -> protein
    in decode_aux triplets []


let () =
    let (rna1:rna) = [G;C;A;G;C;C;G;C;G;G;C;U;G;A;C;G;A;U;U;A;A;U;A;G;U;G;A] in
    let (rna2:rna) = [G;G;A;G;G;C;G;G;G;G;G;U;A;U;A;A;U;C;A;U;U;U;C;A;U;C;C;U;C;G;U;C;U;A;G;U;A;G;C;U;A;A;U;A;G;U;G;A] in
    let (rna3:rna) = [G;U;A;G;U;C;G;U;G;G;U;U;U;A;A;U;A;G;U;G;A] in
    print_list_triplets (generate_bases_triplets rna1);
    print_endline (string_of_protein (decode_arn rna1)) ;
    print_endline "--------------" ;
    print_list_triplets (generate_bases_triplets rna2);
    print_endline (string_of_protein (decode_arn rna2)) ;
    print_endline "--------------" ;
    print_list_triplets (generate_bases_triplets rna3);
    print_endline (string_of_protein (decode_arn rna3))