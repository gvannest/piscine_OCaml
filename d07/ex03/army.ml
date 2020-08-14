class ['a] army (member:'a list) =
    object (self)
        val mutable _member = member

        method add (soldier:'a) = _member <- (soldier :: _member)
        method delete = _member <- try (List.tl _member) with | Failure err -> print_endline "Army is empty!" ; _member
        method armySize = List.length _member
        method armyMembersString = begin
            let rec loop lst = match lst with
                | [] -> print_char '\n'
                | h :: t -> print_endline (h#to_string) ; loop t
            in loop _member
        end

    end