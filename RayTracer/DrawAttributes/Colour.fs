module Colour
open System.Drawing
open System
type Colour =
  | C of float * float * float

let mkColour (r : float) (g : float) (b : float) : Colour = C(r, g, b)
let pow x y = System.Math.Pow (x, y)
///Creates a modular colour from a System.Drawing.Color
///by converting RGB values to a floating point between 0.0 and 1.0
let fromColor (c : Color) : Colour =
    let r = pow ((float c.R) / 255.0) 2.0
    let g = pow ((float c.G) / 255.0) 2.0
    let b = pow ((float c.B) / 255.0) 2.0
    C(r, g, b)

///Produce a System.Drawing.Color from a modular colour
///by converting RGB values to standard int values between 0 and 255
let toColor (C(r,g,b)) = 
    let edge x = 
        match x with 
        | _ when x < 0.0 -> 0.0
        | _ when x > 1.0 -> 1.0
        | _ -> x
    let r' =  int (255.0 *(edge r**0.5))
    let g' =  int (255.0 *(edge g**0.5))
    let b' =  int (255.0 *(edge b**0.5))
    Color.FromArgb(255, r',g', b')

///Takes a colour list and returns a color.
let toColorFromList xs =
    match xs with
    |[] -> System.Drawing.Color.Black
    |v::xs -> toColor v

let getRGB (C(r,g,b)) = (r,g,b)    
    
let scaleColour (r,g,b) (C(ri,gi,bi)) = 
    C(r * ri, g * gi, b * bi)

let merge refl (C(r1,g1,b1)) (C(r2,g2,b2)) = 
    C((refl * r1 + (1.0 - refl) * r2, refl * g1 + (1.0 - refl) * g2, refl * b1 + (1.0 - refl) * b2))
