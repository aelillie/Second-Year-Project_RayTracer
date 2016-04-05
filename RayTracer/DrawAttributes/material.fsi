module Material
open Colour

type Material 

val mkMaterial : Colour -> float -> Material 

val getColour : Material -> Colour
