module type MONOID = sig
    type element
    val zero1 : element
    val zero2 : element
    val mul : element -> element -> element
    val add : element -> element -> element
    val div : element -> element -> element
    val sub : element -> element -> element
end

module INT =
struct
    type element = int
    let zero1 = 0
    let zero2 = 1
    let add = ( + )
    let sub = ( - )
    let mul = ( * )
    let div = ( / )
end

module FLOAT =
struct
    type element = float
    let zero1 = 0.
    let zero2 = 1.
    let add = ( +. )
    let sub = ( -. )
    let mul = ( *. )
    let div = ( /. )
end

module type CALC =
functor (M : MONOID) ->
   sig
    val add : M.element -> M.element -> M.element
    val sub : M.element -> M.element -> M.element
    val mul : M.element -> M.element -> M.element
    val div : M.element -> M.element -> M.element
    val power : M.element -> int -> M.element
    val fact : M.element -> M.element
end

module Calc : CALC =
    functor (M:MONOID) -> struct
        let add x y = M.add x y
        let sub x y = M.sub x y
        let mul x y = M.mul x y
        let div x y = M.div x y

        let rec power (x:M.element) (y:int) = match y with
            | 0                     -> M.zero2
            | 1                     -> x
            | n when n mod 2 = 0    -> let a = power x (n/2) in M.mul a a
            | n                     -> let a = power x (n/2) in M.mul x (M.mul a a)

        let rec fact (x:M.element) = match x with
            | n when n = M.zero1 || n = M.zero2 -> M.zero2
            | n                     -> M.mul n (fact (M.sub n M.zero2))

    end



