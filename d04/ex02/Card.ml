module Color =
struct

    type t = Spade | Heart | Diamond | Club

    let all = [Spade ; Heart ; Diamond ; Club]

    let toString (color:t) = match color with
        | Spade -> "S"
        | Heart -> "H"
        | Diamond -> "D"
        | Club -> "C"

    let toStringVerbose (color:t) = match color with
        | Spade -> "Spade"
        | Heart -> "Heart"
        | Diamond -> "Diamond"
        | Club -> "Club"

end

module Value =
struct

    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

    let toInt (card:t) = match card with
        | T2 -> 1
        | T3 -> 2
        | T4 -> 3
        | T5 -> 4
        | T6 -> 5
        | T7 -> 6
        | T8 -> 7
        | T9 -> 8
        | T10 -> 9
        | Jack -> 10
        | Queen -> 11
        | King -> 12
        | As -> 13

    let toString (card:t) = match card with
        | T2 -> "2"
        | T3 -> "3"
        | T4 -> "4"
        | T5 -> "5"
        | T6 -> "6"
        | T7 -> "7"
        | T8 -> "8"
        | T9 -> "9"
        | T10 -> "10"
        | Jack -> "J"
        | Queen -> "Q" 
        | King -> "K"
        | As -> "A"

    let toStringVerbose (card:t) = match card with
        | T2 -> "2"
        | T3 -> "3"
        | T4 -> "4"
        | T5 -> "5"
        | T6 -> "6"
        | T7 -> "7"
        | T8 -> "8"
        | T9 -> "9"
        | T10 -> "10"
        | Jack -> "Jack"
        | Queen -> "Queen" 
        | King -> "King"
        | As -> "As"

    let next (card:t) = match card with
        | As -> invalid_arg "Error : provided card is As. No next card."
        | _ -> begin
            let rec nextInAll (l:t list) = match l with
                | [] -> invalid_arg "Error: card not found"
                | h :: n :: t when h = card -> n
                | h :: t -> nextInAll t
            in
            nextInAll all
        end

    let previous (card:t) = match card with
        | T2 -> invalid_arg "Error : provided card is T2. No previous card."
        | _ ->
            let rec previousInAll (l:t list) = match l with
                | [] -> invalid_arg "Error: card not found"
                | h :: n :: t when n = card -> h
                | h :: t -> previousInAll t
            in
            previousInAll all

end


type t = {
    value : Value.t;
    color : Color.t;
}

let newCard (v:Value.t) (c:Color.t) = { value = v ; color = c }

let allSpades = 
    let rec createAllSpades (valueList:Value.t list) (result:t list) = match valueList with
        | [] -> List.rev result
        | h :: t ->  createAllSpades t ((newCard h Color.Spade) :: result)
    in
    createAllSpades Value.all []

let allHearts = 
    let rec createAllHearts (valueList:Value.t list) (result:t list) = match valueList with
        | [] -> List.rev result
        | h :: t ->  createAllHearts t ((newCard h Color.Heart) :: result)
    in
    createAllHearts Value.all []

let allDiamonds = 
    let rec createAllDiamonds (valueList:Value.t list) (result:t list) = match valueList with
        | [] -> List.rev result
        | h :: t ->  createAllDiamonds t ((newCard h Color.Diamond) :: result)
    in
    createAllDiamonds Value.all []

let allClubs = 
    let rec createAllClubs (valueList:Value.t list) (result:t list) = match valueList with
        | [] -> List.rev result
        | h :: t ->  createAllClubs t ((newCard h Color.Club) :: result)
    in
    createAllClubs Value.all []


let all = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue (card:t) = card.value
let getColor (card:t) = card.color

let toString (card:t) = Printf.sprintf "%s%s" (Value.toString card.value) (Color.toString card.color)

let toStringVerbose (card:t) = Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose card.value) (Color.toStringVerbose card.color)

let compare (c1:t) (c2:t) = (Value.toInt c1.value) - (Value.toInt c2.value)

let max (c1:t) (c2:t) = if (Value.toInt c1.value) >= (Value.toInt c2.value) then c1 else c2
let min (c1:t) (c2:t) = if (Value.toInt c1.value) <= (Value.toInt c2.value) then c1 else c2

let best (cardList:t list) = match cardList with
    | [] -> invalid_arg "Error: list of cards passed to best is empty."
    | _ -> List.fold_left max (List.hd cardList) (List.tl cardList)

let isOf (card:t) (color:Color.t) = card.color = color
let isSpade (card:t) = card.color = Color.Spade
let isHeart (card:t) = card.color = Color.Heart
let isDiamond (card:t) = card.color = Color.Diamond
let isClub (card:t) = card.color = Color.Club