module Output : sig

    val welcomeMessage : unit -> string
    val turnToPlay : Board.Cell.symbol -> string
    val gridFinished : int -> Board.Cell.symbol -> string
    val playerWinGame : Board.Cell.symbol -> string

end

module Input : sig

    val welcomeResponse : unit -> string
    val userChoice : Board.BigBoard.t -> Board.BoardCell.t -> Board.Cell.symbol ->  Board.Cell.t

end