module Light
open Point
open Colour
open Ray

type Light =
    | L of Point * Colour * float

type AmbientLight =
    | AL of Colour * float

type colour = Colour.Colour

let mkLight (p : Point) (c : Colour) (i : float) : Light = L(p,c,i)
let mkAmbientLight (c : Colour) (i : float) : AmbientLight = AL(c,i)


let getPoint (L(p,c,i)) = p


let calculateI (rd:Vector) (nd:Vector) i = 
    (Vector.dotProduct rd nd) * i
    

let getAmbientI (AL(_,i)) = i

let getLightI (L(p,c,i)) = i
