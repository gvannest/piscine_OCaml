class virtual atom (name:string) (symbol:string) (number:int) =
object (this)
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = number

    method to_string : string = "Atom of " ^ this#name ^ " with symbol " ^ this#symbol ^ " and atomic number " ^ (string_of_int this#atomic_number)
    method equals (that:atom) = (this#name = that#name) && (this#symbol = that#symbol) && (this#atomic_number = that#atomic_number)

end


class hydrogen =
object
    inherit atom "Hydrogen" "H" 1
end

class carbon =
object
    inherit atom "Carbon" "C" 6
end

class oxygen =
object
  inherit atom "Oxygen" "O" 8
end

class helium =
  object
    inherit atom "Helium" "He" 2
  end

class calcium =
object
  inherit atom "Calcium" "Ca" 20
end

class nitrogen =
object
  inherit atom "Nitrogen" "N" 7
end

class aluminium =
object
  inherit atom "Aluminium" "Al" 13
end