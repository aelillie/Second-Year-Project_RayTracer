module Light
open Point
open Material

type Light =
    | L of Point * Colour * float

type AmbientLight =
    | AL of Colour * float

let mkLight (p : Point) (c : Colour) (i : float) : Light = L(p,c,i)
let mkAmbientLight (c : Colour) (i : float) : AmbientLight = AL(c,i)

