let charGenerator () =
    Random.self_init ();
    String.make 1 (char_of_int (Random.int 26 + 97))

let stringGenerator () =
    Random.self_init () ;
    let x = Random.int 4 in match x with
        | 0 -> "Explain! Explain!"
        | 1 -> "Exterminate! Exterminate!"
        | 2 -> "I obey!"
        | 3 -> "You are the Doctor! You are the enemy of the Daleks!"
        | _ -> "Error : Impossible!"

class dalek =
    object (self)
        val _name : string = "Dalek" ^ begin let rec loop x res = match x with
                                | i when i = 3 -> res
                                | _ -> loop (x + 1) (charGenerator () ^ res)
                                in loop 0 "" 
                            end
        val _hp : int = 100
        val mutable _shield : bool = true

        method getName = _name
        method getHp = _hp
        method getShield = _shield
        method setShield b = _shield <- b
        method to_string = "Name : " ^ self#getName ^ "; HP : " ^ (string_of_int self#getHp) ^ "; Shield : " ^ (string_of_bool self#getShield)
        method talk = print_string (stringGenerator ())
        method exterminate (people:People.people) = people#die ; begin let chgShield = if self#getShield = true then false else true in self#setShield chgShield end
        method die = print_string "Emergency Temporal Shift!"
    end