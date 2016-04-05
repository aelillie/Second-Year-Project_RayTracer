module Light
open Point
open Material
open Ray
open Colour

type Light
type AmbientLight

val mkLight : position : Point -> colour : Colour -> intensity : float -> Light
val mkAmbientLight : colour : Colour -> intensity : float -> AmbientLight

val getShadowRay : Point -> Vector -> Light -> float -> Ray
val scaleColour : float * float * float -> Vector -> Vector -> Colour

val applyAL : AmbientLight -> float*float*float -> Colour
