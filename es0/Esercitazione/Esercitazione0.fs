module Esercitazione0

let NomeCognome : string = "Nicolò Veronese"

let pythagoras (c1 : float) (c2 : float) : float = sqrt ((c1 ** 2.0) + (c2 ** 2.0))

let angular_coeff (x1 : float) (y1 : float) (x2 : float) (y2 : float)  : float = (y2 - y1) / (x2 - x1)

let rhombus_perimeter (d1 : float) (d2 : float) : float = 
    let d1_h = d1 / 2.0
    let d2_h = d2 / 2.0
    (pythagoras d1_h d2_h) * 4.0
