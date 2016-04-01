module Light
open Point
open Material

type Light =
    | L of Point * Colour * float

type AmbientLight =
    | AL of Colour * float

let mkLight (p : Point) (c : Colour) (i : float) : Light = failwith "mkLight not implemented"
let mkAmbientLight (c : Colour) (i : float) : AmbientLight = failwith "mkAmbientLight not implemented"
