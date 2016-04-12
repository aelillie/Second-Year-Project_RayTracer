module Colour
open System.Drawing
open System
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
    let edge x = 
        match x with 
        | _ when x < 0.0 -> 0.0
        | _ when x > 1.0 -> 1.0
        | _ -> x
    let r' =  int (255.0 *(edge r))
    let g' =  int (255.0 *(edge g))
    let b' =  int (255.0 *(edge b))

    Color.FromArgb(255, r',g', b')

let getRGB (C(r,g,b)) = (r,g,b)    
    

let scaleColour (C(r,g,b)) i = 
    
    let r' = r * i
    let g' = g * i
    let b' = b * i

    C(r',g',b')

let merge refl (C(r1,g1,b1)) (C(r2,g2,b2)) = 
    C((refl * r1 + (1.0 - refl) * r2, refl * g1 + (1.0 - refl) * g2, refl * b1 + (1.0 - refl) * b2))