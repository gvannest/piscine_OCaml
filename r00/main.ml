let main () =
    print_endline (UserInterface.Output.welcomeMessage ()) ;
    let player = Board.Cell.stringToSym (UserInterface.Input.welcomeResponse ()) in
    let boardGame = Board.BigBoard.emptyBigBoard () in
    let macroBoardInit = Board.BoardCell.emptyBoardCell () in

    let rec game (board:Board.BigBoard.t) (macroBoard:Board.BoardCell.t) (currPlayer:Board.Cell.symbol) =
        print_endline (Board.BigBoard.bigBoardToString board) ;
        print_endline (UserInterface.Output.turnToPlay currPlayer) ;
        let cell = UserInterface.Input.userChoice board macroBoard currPlayer in
        let newBoard = Board.BigBoard.insertCell cell board in
        match Backend.Calc.isGridFinished cell newBoard with
            | x when x = -1 -> game newBoard macroBoard (Backend.Utility.switchUser currPlayer)
            | x -> begin print_endline (UserInterface.Output.gridFinished x currPlayer) ;
                let newMacroBoard = Board.BoardCell.fillMacroBoard macroBoard x currPlayer in
                let modifiedBoard = Board.BigBoard.insertBoardCell (Board.BoardCell.winningBoard currPlayer) x newBoard in
                if Backend.Calc.isGameFinished newMacroBoard currPlayer then begin print_endline (UserInterface.Output.playerWinGame currPlayer) ; print_endline (Board.BigBoard.bigBoardToString modifiedBoard) end
                else game modifiedBoard newMacroBoard (Backend.Utility.switchUser currPlayer)
            end
    in
    game boardGame macroBoardInit player


let () = main ()