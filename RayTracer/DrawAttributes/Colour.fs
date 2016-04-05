module Colour
open System.Drawing

type Colour =
  | C of float * float * float

let mkColour (r : float) (g : float) (b : float) : Colour = C(r, g, b)

let fromColor (c : Color) : Colour =
    let r = (float c.R) / 255.0
    let g = (float c.G) / 255.0
    let b = (float c.B) / 255.0
    C(r, g, b)

let toColor (C(r,g,b)) = 
    let r' = int (r * 255.0)
    let g' = int (g * 255.0)
    let b' = int (b * 255.0)
    Color.FromArgb(255, r',g', b')
    
    

