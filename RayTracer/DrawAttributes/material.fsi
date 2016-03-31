module Material

type Colour
type Material 

val mkColour : r:float -> g:float -> b:float -> Colour
val fromColor : c : System.Drawing.Color -> Colour
val mkMaterial : Colour -> float -> Material 
