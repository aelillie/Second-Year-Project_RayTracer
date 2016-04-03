module Light
open Point
open Material
open Ray

type Light =
    | L of Point * Colour * float

type AmbientLight =
    | AL of Colour * float

let mkLight (p : Point) (c : Colour) (i : float) : Light = L(p,c,i)
let mkAmbientLight (c : Colour) (i : float) : AmbientLight = AL(c,i)


let getShadowRay (p:Point) (rd:Vector) (L(pL,_,_)) (x:float) :Ray = 
    
    let p' = Vector.multScalar rd x |> Point.move p 

    let sr = Point.direction p' pL

    Ray.mkRay 1 1 p' 1.0 sr


let scaleColour (r,g,b) (rd:Vector) (nd:Vector) = 
    
    let s = Vector.dotProduct rd nd

    let r' = r * s
    let g' = g * s
    let b' = b * s

    Material.getColour (Material.mkMaterial (Material.mkColour r' g' b') 1.0)
    

    