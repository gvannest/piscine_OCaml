module Cell : sig
    
    type symbol = X | O | Empty | Space | Slash | Backslash | Vertical
    type point = (int * int)
    type t

    val newPoint : int -> int -> point 
    val newCellPoint : point -> symbol -> t
    val newCellCoor : int -> int -> symbol -> t

    val getCoordinates : t -> point
    val getSymbol : t -> symbol

    val symToString : symbol -> string
    val stringToSym : string -> symbol

end = struct

    type symbol = X | O | Empty | Space | Slash | Backslash | Vertical

    type point = (int * int)

    type t = {
        coordinates : point;
        symbol : symbol;
    }

    let newPoint i j = (i, j) (*i = line / j = col *)

    let newCellPoint (p:point) (s:symbol) = { coordinates = p ; symbol = s }
    let newCellCoor (i:int) (j:int) (s:symbol) = { coordinates = (newPoint i j) ; symbol = s }

    let getCoordinates (cell:t) = cell.coordinates
    let getSymbol (cell:t) = cell.symbol

    let symToString (s:symbol) = match s with
        | X -> "X"
        | O -> "O"
        | Empty -> "-"
        | Space -> " "
        | Slash -> "/"
        | Backslash -> "\\"
        | Vertical -> "|"

    let stringToSym (str:string) = match str with
        | "X" -> X
        | "O" -> O
        | "-" -> Empty
        | _ -> failwith "Error on string symbol"

end

module BoardCell : sig

    type t

    val insertCell : Cell.t -> t -> t
    val emptyBoardCell : unit -> t
    val winningBoard : Cell.symbol -> t
    val boardCellLineToString : t -> int -> string
    val boardCellToString : t -> string
    val getCell : Cell.point -> t -> Cell.t
    val isFull : t -> bool
    val fillMacroBoard  : t -> int -> Cell.symbol -> t
    val filter : (Cell.t -> bool) -> t -> (Cell.t list)

end = struct

    type t = Cell.t list

    let emptyBoardCell () =
        let rec emptyBoardCell_aux (cell:Cell.t) (result:t) = match Cell.getCoordinates cell with
            | (i, j) when i = 2 && j = 2 -> List.rev (cell :: result)
            | (i, j) when j = 3 -> emptyBoardCell_aux (Cell.newCellCoor (i + 1) 0 Cell.Empty) result
            | (i, j) -> emptyBoardCell_aux (Cell.newCellCoor i (j + 1) Cell.Empty) (cell :: result)
        in emptyBoardCell_aux (Cell.newCellCoor 0 0 Cell.Empty) []

    let newBoardO () =
        let rec loop (coor:Cell.point) (result:t) = match coor with
            | (i, j) when i = 3 -> List.rev result
            | (i, j) when j = 3 -> loop (Cell.newPoint (i + 1) 0) result
            | (i, j) when i = j -> begin
                if i = 1 then loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Space) :: result)
                else loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Slash) :: result)
            end
            | (i, j) when i = 1 -> loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Vertical) :: result)
            | (i, j) when j = 1 -> loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Empty) :: result)
            | (i, j)  -> loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Backslash) :: result)
        in loop (Cell.newPoint 0 0) []

    let newBoardX () =
        let rec loop (coor:Cell.point) (result:t) = match coor with
            | (i, j) when i = 3 -> List.rev result
            | (i, j) when j = 3 -> loop (Cell.newPoint (i + 1) 0) result
            | (i, j) when i = j -> begin
                if i = 1 then loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.X) :: result)
                else loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Backslash) :: result)
            end
            | (i, j) when (i, j) = (2, 0) || (i, j) = (0, 2) -> loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Slash) :: result)
            | (i, j) -> loop (Cell.newPoint i (j + 1)) ((Cell.newCellPoint coor Cell.Space) :: result)
        in loop (Cell.newPoint 0 0) []
    
    let winningBoard (player:Cell.symbol) = match player with
        | Cell.O -> newBoardO ()
        | Cell.X -> newBoardX ()
        | _ -> failwith "Error in BoardCell.winningBoard -> player does not match any"

    let insertCell (cell:Cell.t) (board:t) =
        let rec insertCell_aux (currentBoard:t) (resultBoard:t) = match currentBoard with
            | [] -> raise (Failure "Error in Board.BoardCell.insertCell : no place found for cell to be inserted")
            | h :: tail when Cell.getCoordinates h = Cell.getCoordinates cell -> (List.rev (cell :: resultBoard)) @ tail
            | h :: tail -> insertCell_aux tail (h :: resultBoard)
        in
        insertCell_aux board []

    let rec isFull (current_board:t) = match current_board with
        | [] -> true
        | h :: t when Cell.getSymbol h = Cell.Empty -> false
        | h :: t -> isFull t


    let boardCellLineToString (board:t) (line:int) =
        let rec boardLineToString_aux (currentBoard:t) (result:string) = match currentBoard with
            | [] -> result
            | head :: tail -> match Cell.getCoordinates head with
                | (i, j) when i = line ->
                    if j < 2 then boardLineToString_aux tail (result ^ " " ^ (Cell.symToString (Cell.getSymbol head)))
                    else boardLineToString_aux [] (result ^ " " ^ (Cell.symToString (Cell.getSymbol head)))
                | _ -> boardLineToString_aux tail result
        in boardLineToString_aux board ""
    

    let boardCellToString (board:t) =
        let rec loop (line:int) (result:string) = match line with
            | x when x = 3 -> Printf.sprintf "%s\n" result
            | _ -> loop (line + 1) (Printf.sprintf "%s\n%s" result (boardCellLineToString board line))
        in loop 0 ""


    let rec getCell (coor:Cell.point) (boardCell:t) = match boardCell with
        | [] -> failwith "Error : cell not found. getCell in Board.BoardCell"
        | h :: t when Cell.getCoordinates h = coor -> h
        | h :: t -> getCell coor t

    let fillMacroBoard  (board:t) (boardIdx:int) (s:Cell.symbol) =
        let i = boardIdx / 3 in let j = boardIdx mod 3 in
        let cell = Cell.newCellCoor i j s in
        insertCell cell board

    let filter (ft:(Cell.t -> bool)) (board:t) =
        List.filter ft board

end

module BigBoard : sig

    type t

    val boardLength : t -> int
    val emptyBigBoard : unit -> t
    val insertCell : Cell.t -> t -> t
    val insertBoardCell : BoardCell.t -> int -> t -> t
    val boardToListString : t -> string list
    val bigBoardToString : t -> string
    val getCell : Cell.point -> t -> Cell.t
    val getBoardNumber : Cell.point -> int
    val getBoardCell : Cell.point -> t -> BoardCell.t
    val isCellEmpty : Cell.point -> t -> bool
    val isBoardCellAvail : Cell.point -> t -> BoardCell.t -> bool


end = struct

    type t = BoardCell.t list

    let boardLength (board:t) = List.length board

    let emptyBigBoard () = 
        let rec loop (n:int) (result:t) = match n with
            | x when x = 9 -> result
            | _ -> loop (n + 1) (BoardCell.emptyBoardCell() :: result)
        in loop 0 []


    let getBoardNumber (coor:Cell.point) = match coor with
        | (i, j) when j < 3 && i < 3 -> 0
        | (i, j) when j < 6 && i < 3 -> 1
        | (i, j) when j < 9 && i < 3 -> 2
        | (i, j) when j < 3 && i < 6 -> 3
        | (i, j) when j < 6 && i < 6 -> 4
        | (i, j) when j < 9 && i < 6 -> 5
        | (i, j) when j < 3 && i < 9 -> 6
        | (i, j) when j < 6 && i < 9 -> 7
        | (i, j) when j < 9 && i < 9 -> 8
        | _ -> failwith "Error in getBoardNumber; Board module"


    let convertCellLocal (cell:Cell.t) = 
        let cellCoordinates = Cell.getCoordinates cell in
        let newCoordinates = Cell.newPoint ((fst cellCoordinates) mod 3) ((snd cellCoordinates) mod 3) in
        Cell.newCellPoint newCoordinates (Cell.getSymbol cell)


    let insertCell (cell:Cell.t) (bigBoard:t) =
        let boardNumber = getBoardNumber (Cell.getCoordinates cell) in
        let cellToInsert = convertCellLocal cell in
        let newBoardCell = BoardCell.insertCell cellToInsert (List.nth bigBoard boardNumber) in
        let buildBoardFun (i:int) (board:BoardCell.t) = if i = boardNumber then newBoardCell else board in
        List.mapi buildBoardFun bigBoard


    let insertBoardCell (boardCell:BoardCell.t) (position:int) (bigBoard:t) =
        let matchIdx = fun (x:int) (board:BoardCell.t) -> if x = position then boardCell else board in
        List.mapi matchIdx bigBoard


    let boardToListString (bigBoard:t) =
        let rec lineToString (line:int) (boardIdx:int) (result:string) = match boardIdx with
            | x when x mod 3 = 2  -> (Printf.sprintf "%s%s" result (BoardCell.boardCellLineToString (List.nth bigBoard boardIdx) line))
            | _ -> lineToString line (boardIdx + 1) (Printf.sprintf "%s%s |" result (BoardCell.boardCellLineToString (List.nth bigBoard boardIdx) line))
        in
        let rec listOfBlocLines (line:int) (boardIdx:int) (result:string list) = match line with
            | x when x = 3 -> List.rev result
            | _ -> listOfBlocLines (line + 1) boardIdx ((lineToString line boardIdx "") :: result)
        in
        let rec allLines (boardIdx:int) (result:string list) = match boardIdx with
            | x when x = 6 -> let newBloc = listOfBlocLines 0 boardIdx [] in (result @ newBloc)
            | _ -> let newBloc = listOfBlocLines 0 boardIdx [] in allLines (boardIdx + 3) (result @ newBloc @ [(String.make (String.length (List.hd newBloc)) '-')])
        in
        allLines 0 []


    let bigBoardToString (bigBoard:t) =
        let bigBoardListString = boardToListString bigBoard
        in
        let rec loop (boardString:string list) (result:string) = match boardString with
            | [] -> result
            | h :: t -> loop t (Printf.sprintf "%s%s\n" result h)
        in
        loop bigBoardListString "\n" 

    let getCell (coor:Cell.point) (board:t) =
        let boardNumber = getBoardNumber coor in
        let localCellToFind = convertCellLocal (Cell.newCellPoint coor Cell.Empty) in
        let localCell = BoardCell.getCell (Cell.getCoordinates localCellToFind) (List.nth board boardNumber) in
        Cell.newCellPoint coor (Cell.getSymbol localCell)

    let getBoardCell (coor:Cell.point) (board:t) =
        let boardNumber = getBoardNumber coor in
        List.nth board boardNumber


    let isCellEmpty (coor:Cell.point) (board:t) =
        let cell = getCell coor board in match (Cell.getSymbol cell) with
            | Cell.Empty -> true
            | _ -> false

    let isBoardCellAvail (coor:Cell.point) (board:t) (macroBoard:BoardCell.t)=
        let boardNumber = getBoardNumber coor in
        let coorInMacroBoard = Cell.newPoint (boardNumber / 3) (boardNumber mod 3) in
        let correspondingCellInMacro = BoardCell.getCell coorInMacroBoard macroBoard in
        if Cell.getSymbol correspondingCellInMacro = Cell.Empty then true else false


end