module Material
open Colour

type Material 

///Produce a material from given properties
val mkMaterial : Colour -> float -> Material 

///Extract the colour of a Material
val getColour : Material -> Colour

///Extract the reflection value of a Material
val getReflection : Material -> float
