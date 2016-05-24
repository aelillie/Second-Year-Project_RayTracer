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


let calculateI (nd:Vector) (sd:Vector) = 
    (Vector.dotProduct nd sd) 
    

let getAmbientI (AL(c,i)) = 
    let (r,g,b) = Colour.getRGB c
    (r*i, g*i, b*i)

let getLightI (L(p,c,i)) = i

let getColourI (L(p,c,i)) angleI = 
    let i' = i * angleI
    let (r,g,b) = Colour.getRGB c
    (r*i', g*i', b*i')
                        


let getLightC (L(_,c,_)) = c
