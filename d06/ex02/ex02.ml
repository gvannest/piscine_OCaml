module type PAIR = sig 
    val pair : (int * int)
end

module type VAL = sig
    val x : int
end

module type MAKEFSTSND =
    functor (Pair : PAIR) -> VAL

module MakeFst : MAKEFSTSND =
    functor (Pair : PAIR) ->
    struct
        let x = fst Pair.pair
    end

module MakeSnd : MAKEFSTSND =
    functor (Pair : PAIR) ->
    struct
        let x = snd Pair.pair
    end

module Pair : PAIR = struct
    let pair = ( 21, 42 )
end 

module Fst : VAL = MakeFst (Pair)

module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x