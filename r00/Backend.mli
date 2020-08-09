module Utility : sig

    type intOption = Some of int | None
    
    val switchUser : Board.Cell.symbol -> Board.Cell.symbol
    val safeConvToInt : string -> intOption
    val setCoor : int -> int -> Board.Cell.point
    val getValueIntOption : intOption -> int

end

module Calc : sig

    val isGridFinished : Board.Cell.t -> Board.BigBoard.t -> int
    val isGameFinished : Board.BoardCell.t -> Board.Cell.symbol -> bool

end