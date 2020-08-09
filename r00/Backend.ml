module Utility : sig

    type intOption = Some of int | None

    val switchUser : Board.Cell.symbol -> Board.Cell.symbol
    val safeConvToInt : string -> intOption
    val setCoor : int -> int -> Board.Cell.point
    val getValueIntOption : intOption -> int


end = struct

    type intOption = Some of int | None

    let getValueIntOption x = match x with
        | Some n -> n
        | None -> failwith "Error in conception : getValueIntOption should not be called on None"

    let switchUser s = match s with
        | Board.Cell.X -> Board.Cell.O
        | Board.Cell.O -> Board.Cell.X
        | _ -> failwith "Error in switchUser : Unknown user"

    (* let safeConvToInt (str:string) =
        try Some (int_of_string str)
        with Failure "int_of_string" -> None *)

    let safeConvToInt (str:string) = match int_of_string str with
        | exception (Failure _) -> None
        | _ -> Some (int_of_string str)
    

    let setCoor x y = Board.Cell.newPoint (x - 1) (y - 1)
        
end

module Calc : sig

    
    val checkListSymbol : Board.Cell.t list -> Board.Cell.symbol -> bool

    val isGridFinished : Board.Cell.t -> Board.BigBoard.t -> int
    val isGameFinished : Board.BoardCell.t -> Board.Cell.symbol -> bool


end = struct

    let extractLns (board:Board.BoardCell.t) =
        let rec loop (line:int) (result:(Board.Cell.t list) list)= match line with
            | 3 -> result
            | _ -> begin
                let filterLines (cell:Board.Cell.t) =
                    let (i, j) = Board.Cell.getCoordinates cell in if i = line then true else false
                in
                loop (line + 1) ((Board.BoardCell.filter filterLines board) :: result)
            end
        in
        loop 0 []

    let extractCols (board:Board.BoardCell.t) =
        let rec loop (cols:int) (result:(Board.Cell.t list) list)= match cols with
            | 3 -> result
            | _ -> begin
                let filterCols (cell:Board.Cell.t) =
                    let (i, j) = Board.Cell.getCoordinates cell in if j = cols then true else false
                in
                loop (cols + 1) ((Board.BoardCell.filter filterCols board) :: result)
            end
        in
        loop 0 []


    let extractDiagos (board:Board.BoardCell.t) =
        let filterDiago1 (cell:Board.Cell.t) = 
            let (i, j) = Board.Cell.getCoordinates cell in if i = j then true else false
        in
        let filterDiago2 (cell:Board.Cell.t) = 
            let (i, j) = Board.Cell.getCoordinates cell in if (i + j) = 2 then true else false
        in
        let list1 = Board.BoardCell.filter filterDiago1 board in
        let list2 = Board.BoardCell.filter filterDiago2 board in
        [list1; list2]


    let checkListSymbol (lst:Board.Cell.t list) (s:Board.Cell.symbol) =
        let rec loop current_list = match current_list with
            | [] -> true
            | h :: t -> if Board.Cell.getSymbol h = s then loop t else false
        in loop lst

    let isWon (board:Board.BoardCell.t) (s:Board.Cell.symbol) = 
        let listToCheck = (extractLns board) @ (extractCols board) @ (extractDiagos board) in
        let rec loop (lst:(Board.Cell.t list) list) = match lst with
            | [] -> false
            | h :: t -> if checkListSymbol h s = true then true else loop t
        in
        loop listToCheck


    let isGridFinished (cell:Board.Cell.t) (board:Board.BigBoard.t) =
        let boardNumber = Board.BigBoard.getBoardNumber (Board.Cell.getCoordinates cell) in
        let current_board = Board.BigBoard.getBoardCell (Board.Cell.getCoordinates cell) board in
        if isWon current_board (Board.Cell.getSymbol cell) || Board.BoardCell.isFull current_board then boardNumber else (-1)

    let isGameFinished (macroBoard:Board.BoardCell.t) (player:Board.Cell.symbol) =
        if isWon macroBoard player || Board.BoardCell.isFull macroBoard then true else false

    

end