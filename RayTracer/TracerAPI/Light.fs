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


let getShadowRay (p:Point) (rd:Vector) (L(pL,_,_)) (x:float) :Ray = 
    
    let p' = Vector.multScalar rd x |> Point.move p 

    let sr = Point.direction p' pL

    Ray.mkRay p' 1.0 sr


let scaleColour (r,g,b) (rd:Vector) (nd:Vector) = 
    
    let s = Vector.dotProduct rd nd

    let check s = 
        if s > 180.0 then 360.0 - s else s
    let s = check s

    let r' = r * s
    let g' = g * s
    let b' = b * s

    Material.getColour (Material.mkMaterial (Colour.mkColour r' g' b') 1.0)
    

let applyAL (AL(c,i)) (r,g,b) = 
    
    
    let r' = r * i
    let g' = g * i
    let b' = b * i  
    
    Material.getColour (Material.mkMaterial (Colour.mkColour r' g' b') 1.0)  