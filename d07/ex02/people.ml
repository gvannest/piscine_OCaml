class people name =
    object (self)
        val _name : string = name
        val _hp : int = 100

        method get_name = _name
        method get_hp = _hp
        method to_string = "Name : " ^ self#get_name ^ " / Hp = " ^ (string_of_int self#get_hp)
        method talk = print_string ("I'm " ^ self#get_name ^ "! Do you know the Doctor?")
        method die = print_string "Aaaarghh!"
        initializer print_endline ("People named " ^ self#get_name ^ " has been created with a default hp of 100")

    end