
let printListString (listString:string list) = match listString with
    | [] -> print_string "[]"
    | _ -> begin
        print_char '[' ;
        let rec print_aux lst = match lst with
            | [] -> print_endline "]"
            | h :: t -> print_string h ; print_string ", " ; print_aux t
        in
        print_aux listString
    end

let printCouple (couple: Deck.Card.t * Deck.t) = match couple with
    | (h, t) -> Printf.printf "(%s, " (Deck.Card.toStringVerbose h) ; printListString (Deck.toStringListVerbose t) ; print_endline ")\n"

let main () =
 
    let newDeck = Deck.newDeck () in
    printListString (Deck.toStringList newDeck) ;
    print_char '\n' ;
    print_char '\n' ;
    let newDeck2 = Deck.newDeck () in
    printListString (Deck.toStringList newDeck2) ;
    print_char '\n' ;
    print_char '\n' ;

    printCouple (Deck.drawCard newDeck2)

let () = main ()