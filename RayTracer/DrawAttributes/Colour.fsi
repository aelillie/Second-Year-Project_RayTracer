module colour

type colour

val mkColour : r:float -> g:float -> b:float -> colour

val fromColor : c : System.Drawing.Color -> colour
