module Esercitazione4

let NomeCognome : string = "Nicolò Veronese"

let rec coprime (x : int) (y : int) : bool = 
    let rec gcd (x : int) (y : int) : int = 
        if y = 0 then
            x
        else if x % y = 0 then
            y
        else
            gcd y (x % y)
    gcd x y = 1

let is_prime (n : int) =
    let rec is_prime_rec (n : int) (d : int) = 
        if d = 1 then true else if n % d = 0 then false else is_prime_rec n (d - 1)
    if n < 2 then false else is_prime_rec n (n / 2)

let rec goldbach (n : int) : int * int = 
    let rec goldbach_ric (max : int) (down : int) (up: int) : int * int = 
        if is_prime down && is_prime up then (down, up)
        else 
            let low = (down + 1)
            goldbach_ric max (low) (max - low)
    goldbach_ric n 2 (n - 2)
    //let half = n / 2
    //if is_prime half then
    //    (half, half)
   // else
        

let rec crescente (l : int list) : bool = 
    let rec crescente_rec (old :int) (l : int list) =
        match l with
            x :: xs -> (old <= x) && crescente_rec x xs
            | [] -> true
    match l with x::xs -> crescente_rec x xs | [] -> true
         
