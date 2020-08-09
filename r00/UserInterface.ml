module Output : sig

    val welcomeMessage : unit -> string
    val turnToPlay : Board.Cell.symbol -> string
    val gridFinished : int -> Board.Cell.symbol -> string
    val playerWinGame : Board.Cell.symbol -> string

end = struct

    let welcomeMessage () = Printf.sprintf "Hello and welcome to our Super Tictactoe!\nPlease choose which player is starting (X or O) :"
    
    let turnToPlay (player:Board.Cell.symbol) =
        Printf.sprintf "%s's turn to play." (Board.Cell.symToString player)

    let gridFinished x s =  Printf.sprintf "%s wins grid %d!" (Board.Cell.symToString s) (x + 1)

    let playerWinGame s = Printf.sprintf "%s wins the game!" (Board.Cell.symToString s)


end

module Input : sig

    val welcomeResponse : unit -> string
    val userChoice : Board.BigBoard.t -> Board.BoardCell.t -> Board.Cell.symbol ->  Board.Cell.t

end = struct

    let rec welcomeResponse () =
        let resp = read_line () in
        let len = String.length resp in
        match len with
            | x when x <> 1 -> begin print_endline "Illegal format : Please choose which player is starting (X or O) :" ; welcomeResponse () end
            | _ -> begin 
                match resp with
                    | c when c = "X" || c = "O" -> resp
                    | _ -> print_endline "Unavailable choice : Please choose which player is starting (X or O) :" ; welcomeResponse ()
            end

    let rec userChoice (board:Board.BigBoard.t) (macro:Board.BoardCell.t) (player:Board.Cell.symbol) = 
        let lenBoard = Board.BigBoard.boardLength board in
        let resp = read_line () in
        let inputList = String.split_on_char ' ' resp in
        match (List.length inputList) with
            | x when x <> 2 -> begin print_endline "Incorrect format." ; userChoice board macro player end
            | _ -> begin
                let first = List.hd inputList in let second = List.nth inputList 1 in
                let choice = (Backend.Utility.safeConvToInt first, Backend.Utility.safeConvToInt second) in match choice with
                    | (i, j) when i = None || j = None -> begin print_endline "Incorrect format." ; userChoice board macro player end
                    | _ -> begin
                        let i = (Backend.Utility.getValueIntOption (fst choice)) in let j = (Backend.Utility.getValueIntOption (snd choice)) in
                        let coor = Backend.Utility.setCoor i j in
                        if i <= 0 ||i > lenBoard || j<=0 || j > lenBoard then begin print_endline "Illegal move." ; userChoice board macro player end
                        else if (Board.BigBoard.isCellEmpty coor board = false) || (Board.BigBoard.isBoardCellAvail coor board macro = false) then begin print_endline "Illegal move." ; userChoice board macro player end
                        else Board.Cell.newCellPoint coor player
                    end
            end


end