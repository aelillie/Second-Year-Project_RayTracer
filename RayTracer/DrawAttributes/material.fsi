module Material

type Colour
type Material 

val mkColour : r:float -> g:float -> b:float -> Colour
val fromColor : c : System.Drawing.Color -> Colour
val getColour : Material -> System.Drawing.Color
val mkMaterial : Colour -> float -> Material 
