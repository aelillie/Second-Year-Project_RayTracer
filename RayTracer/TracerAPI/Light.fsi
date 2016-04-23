module Light
open Point
open Material
open Ray
open Colour

type Light
type AmbientLight

val mkLight : position : Point -> colour : Colour -> intensity : float -> Light
val mkAmbientLight : colour : Colour -> intensity : float -> AmbientLight

//Calculates the intensity the light has on a point, given the direction of the shadowRay and the original rays direction.
val calculateI :  Vector -> Vector -> float

val getPoint : Light -> Point
val getAmbientI : AmbientLight -> (float*float*float)
val getLightI : Light -> float
val getLightC : Light -> Colour
val getColourI : Light -> float -> (float * float * float)
