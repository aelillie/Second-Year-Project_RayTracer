module Light
open Point
open Material
open Ray
open Colour

type Light
type AmbientLight

val mkLight : position : Point -> colour : Colour -> intensity : float -> Light
val mkAmbientLight : colour : Colour -> intensity : float -> AmbientLight

///Calculates the intensity the light has on a point, given the direction of the shadowRay and the original rays direction.
val calculateI :  Vector -> Vector -> float

///Returns point of a light
val getPoint : Light -> Point

///Returns intensity of ambient light
val getAmbientI : AmbientLight -> (float*float*float)
val getLightI : Light -> float

///Returns the Color of the light as a colour type
val getLightC : Light -> Colour

///Scales the color values with a float
val getColourI : Light -> float -> (float * float * float)
