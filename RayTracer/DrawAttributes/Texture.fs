module Texture
open Material
open System.Drawing
open Colour

type Texture =
    | T of (float -> float -> Material)

//Creates a Texture
let mkTexture (f : float -> float -> Material) : Texture = T(f)

//Takes a Materiale and make a Texture from that
let mkMatTexture (m : Material) : Texture = mkTexture (fun x y -> m)

//Function, that takes a function, for finding the Material at an exact pixel
let getMaterialAtPoint (T(f)) x y = f x y

//Takes two floats and a string and finds the Material at a specific pixel in the Bitmap and returns a Texture 
let mkTextureFromFile (tr : float -> float -> float * float) (file : string) =
    let img = new Bitmap(file)
    let width = img.Width - 1
    let height = img.Height - 1
    let widthf = float width
    let heightf = float height
    let texture x y =
      let (x', y') = tr x y
      let x'', y'' = int (widthf * x'), int (heightf * y')
      let c = lock img (fun () -> img.GetPixel(x'',y''))
      mkMaterial (fromColor c) 0.0
    T(texture)






 


