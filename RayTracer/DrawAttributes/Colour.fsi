module Colour
type Colour

val mkColour : r:float -> g:float -> b:float -> Colour
val fromColor : c : System.Drawing.Color -> Colour
val toColor : Colour -> System.Drawing.Color
val toColorFromList : Colour list -> System.Drawing.Color
val getRGB : Colour -> float * float * float
val merge : float -> Colour -> Colour -> Colour
val scaleColour : (float*float*float) -> Colour -> Colour