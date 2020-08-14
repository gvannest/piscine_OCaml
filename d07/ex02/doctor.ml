let draw_tardis () = 
print_endline "
            ___        \n 
    _______(_@_)_______\n
    | POLICE      BOX |\n
    |_________________|\n
     | _____ | _____ | \n
     | |###| | |###| | \n
     | |###| | |###| | \n
     | _____ | _____ | \n
     | || || | || || | \n
     | ||_|| | ||_|| | \n
     | _____ |$_____ | \n
     | || || | || || | \n
     | ||_|| | ||_|| | \n
     | _____ | _____ | \n
     | || || | || || | \n
     | ||_|| | ||_|| | \n
     |       |       | \n
     ***************** \n"

class doctor (name:string) (age:int) (sidekick:People.people) =
    object (self)
        val _name = name
        val mutable _age = age
        val _sidekick = sidekick
        val mutable _hp = 100

        method getName = _name
        method getAge = _age
        method getSidekick = _sidekick
        method getHp = _hp
        method setHp x = _hp <- x
        method to_string = "Doctor " ^ self#getName ^ ", aged " ^ (string_of_int self#getAge) ^ ", sidekick " ^ (self#getSidekick)#to_string ^ ", with hp " ^ (string_of_int self#getHp)
        method talk = print_string "Hi! I’m the Doctor!"
        initializer print_endline ("Doctor named " ^ self#getName ^ " is born!")
        method travel_in_time (start:int) (arrival:int) = _age <- (_age + start + arrival) ; draw_tardis ()
        method use_sonic_screwdriver = print_string "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
        method private regenerate = _hp <- 100
        method test_regenerate = self#regenerate
    end