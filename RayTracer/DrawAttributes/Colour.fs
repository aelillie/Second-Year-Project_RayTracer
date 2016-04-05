﻿module Colour
open System.Drawing

type Colour =
  | C of float * float * float

let mkColour (r : float) (g : float) (b : float) : Colour = C(r, g, b)

///Creates a modular colour from a System.Drawing.Color
///by converting RGB values to a floating point between 0.0 and 1.0
let fromColor (c : Color) : Colour =
    let r = (float c.R) / 255.0
    let g = (float c.G) / 255.0
    let b = (float c.B) / 255.0
    C(r, g, b)

///Produce a System.Drawing.Color from a modular colour
///by converting RGB values to standard int values between 0 and 255
let toColor (C(r,g,b)) = 
    let r' = int (r * 255.0)
    let g' = int (g * 255.0)
    let b' = int (b * 255.0)
    Color.FromArgb(255, r',g', b')
    
    

