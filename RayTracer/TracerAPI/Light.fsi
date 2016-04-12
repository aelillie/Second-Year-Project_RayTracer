module Light
open Point
open Material
open Ray
open Colour

type Light
type AmbientLight

val mkLight : position : Point -> colour : Colour -> intensity : float -> Light
val mkAmbientLight : colour : Colour -> intensity : float -> AmbientLight

val calculateI :  Vector -> Vector -> float -> float

val getPoint : Light -> Point
val getAmbientI : AmbientLight -> float
val getLightI : Light -> float
