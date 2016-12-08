module Esercitazione1

let NomeCognome : string = "Nicolò Veronese"

let fahrenheit_to_celsius (deg : float) : float = (deg - 32.0) * (5.0 / 9.0);

let euclides_ipotenusa (cb : float) (ch : float) : float = float(cb ** 2.0) / ch

let triangle_type (a : int) (b : int) (c : int) : char = 
    if (a = b && b = c) then 'E'
    elif (a = b && b <> c) || (b = c && a <> b)  || (a = c && a <> b) then 'I'
    else 'S'