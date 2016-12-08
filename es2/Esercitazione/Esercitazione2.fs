module Esercitazione2

let NomeCognome : string = "Nicolò Veronese"

let prezzo (prodotto : string) : float = 
    match prodotto with
    | "Taralli" -> 0.50
    | "Patatine" -> 0.65
    | "Acqua" -> 0.50
    | "Coca cola" -> 0.80
    | "Duplo" -> 1.00
    | _ -> failwith "Prodotto non disponibile"


let resto (prodotto : string) (credito : float) : float = 
    let przz = prezzo(prodotto)
    if credito < przz then
            failwith "Credito insufficiente"
    else
            credito - przz

let encode_char (ch : char) (n : int) : char =
    let offset = int (ch) + n
    if n >= 0 then
        char((offset - int('a')) % 26 + int('a'))
    else
        char((offset - int('z')) % 26 + int('z'))