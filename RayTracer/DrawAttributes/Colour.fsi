module Colour
type Colour

val mkColour : r:float -> g:float -> b:float -> Colour
val fromColor : c : System.Drawing.Color -> Colour
val toColor : Colour -> System.Drawing.Color
val getRGB : Colour -> float * float * float
