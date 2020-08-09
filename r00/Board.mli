module Cell : sig
    
    type symbol = X | O | Empty | Space | Slash | Backslash | Vertical
    type point = (int * int)
    type t


    val newPoint : int -> int -> point 
    val newCellCoor : int -> int -> symbol -> t
    val newCellPoint : point -> symbol -> t
    val stringToSym : string -> symbol
    val symToString : symbol -> string
    val getCoordinates : t -> point
    val getSymbol : t -> symbol


end

module BoardCell : sig

    type t

    val emptyBoardCell : unit -> t
    val winningBoard : Cell.symbol -> t
    val insertCell : Cell.t -> t -> t
    val boardCellToString : t -> string
    val fillMacroBoard  : t -> int -> Cell.symbol -> t
    val isFull : t -> bool
    val filter : (Cell.t -> bool) -> t -> (Cell.t list)


end

module BigBoard : sig

    type t

    val boardLength : t -> int
    val emptyBigBoard : unit -> t
    val insertCell : Cell.t -> t -> t
    val insertBoardCell : BoardCell.t -> int -> t -> t
    val boardToListString : t -> string list
    val bigBoardToString : t -> string
    val isCellEmpty : Cell.point -> t -> bool
    val isBoardCellAvail : Cell.point -> t -> BoardCell.t -> bool
    val getBoardCell : Cell.point -> t -> BoardCell.t
    val getBoardNumber : Cell.point -> int

end