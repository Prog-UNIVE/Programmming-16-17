module Esercitazione6

let NomeCognome : string = "Nicolò Veronese"

let rec prefix (l1 : int list) (l2 : int list) : bool = match l1,l2 with
                                                        [],[]->true
                                                        |[],l2->true
                                                        |l1,[]->false
                                                        |x::xs,y::ys-> if(x=y)then prefix xs ys else false
let rec contaelementi(l:int list):int = match l with
                                        []->0
                                        |[x]->1
                                        |x::xs->1+contaelementi(xs)

let rec auxalternate(l:int list)(ris1:int list)(ris2:int list)(count:int):(int list*int list)= match l,ris1,ris2,count with
                                                                                    [],[],[],count->([],[])
                                                                                    |[],ris1,ris2,count -> (ris1,ris2)
                                                                                    |x::xs,ris1,ris2,count->if(count%2<>0)then auxalternate xs (ris1@[x]) (ris2) (count+1) else auxalternate xs (ris1) (ris2@[x])(count+1)



let alternate (l : int list) : (int list * int list) = auxalternate l [] [] 1

let rec getultimo (l:int list):int=match l with
                                    []-> -1
                                    |[x]->x
                                    |x::xs->getultimo(xs)


let rec eliminasottolista(l:int list)(sublist:int list):int list = match l,sublist with
                                                                    [],[]->[]
                                                                    |[x],[]->[x]
                                                                    |l,[]->l
                                                                    |x::xs,y::ys->eliminasottolista(xs)(ys)
                                                                    |_->failwith "err eliminasotto"
let rec getsub(l:int list)(ris:int list):int list = match l,ris with
                                                         [],[]->[]
                                                        |[x],[]->[x]
                                                        |[],ris->ris
                                                        |[x],ris->ris@[x]
                                                        |x::y::xs,[]->if(x<=y) then getsub(y::xs)([x]) else getsub([])([x])
                                                        |x::y::xs,ris->if(x<=y) then getsub(y::xs)(ris@[x]) else getsub([])(ris@[x])


let rec terzo(l:int list): (int list list ) = match l with 
                                                          []->[]
                                                          |l ->if(getsub(l)([])<>[]) then getsub(l)([])::(terzo (eliminasottolista (l)(getsub(l)([])))) else (terzo (eliminasottolista (l)(getsub(l)([]))))


let sub_ord (l : int list) : int list list = terzo l
