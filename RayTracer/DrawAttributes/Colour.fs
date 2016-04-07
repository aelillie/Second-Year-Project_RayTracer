module Colour
open System.Drawing
open System
type Colour =
  | C of float * float * float

let mkColour (r : float) (g : float) (b : float) : Colour = C(r, g, b)

///Creates a modular colour from a System.Drawing.Color
///by converting RGB values to a floating point between 0.0 and 1.0
let fromColor (c : Color) : Colour =
    let r = System.Math.Pow(((float c.R) / 255.0),2.0)
    let g = System.Math.Pow(((float c.G) / 255.0),2.0)
    let b = System.Math.Pow(((float c.B) / 255.0),2.0)  
    C(r, g, b)

///Produce a System.Drawing.Color from a modular colour
///by converting RGB values to standard int values between 0 and 255
let toColor (C(r,g,b)) = 
    let edge x = 
        match x with 
        | _ when x < 0.0 -> 0.0
        | _ when x > 1.0 -> 1.0
        | _ -> x
    let r' =  int (255.0 *(System.Math.Pow((edge r),0.5)))
    let g' =  int (255.0 *(System.Math.Pow((edge g),0.5)))
    let b' =  int (255.0 *(System.Math.Pow((edge b),0.5)))

    Color.FromArgb(255, r',g', b')

let getRGB (C(r,g,b)) = (r,g,b)    
    

let scaleColour (C(r,g,b)) i = 
    
    let r' = r * i
    let g' = g * i
    let b' = b * i

    C(r',g',b')

