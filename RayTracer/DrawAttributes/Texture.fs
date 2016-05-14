module Texture
open Material
open System.Drawing

type Texture =
    | T of (float -> float -> Material)


let mkTexture (f : float -> float -> Material) : Texture = T(f)

let mkMatTexture (m : Material) : Texture = mkTexture (fun x y -> m)

let getMaterialAtPoint (T(f)) x y = f x y

let mkTextureFromFile (tr : float -> float -> float * float) (file : string) (reflection:float) =
    let img = new Bitmap(file)
    let widthf = float (img.Width - 1)
    let heightf = float (img.Height - 1)
    let texture u v =
      let color = img.GetPixel (int (widthf * u), int (heightf * (1.0-v)))
      mkMaterial (Colour.fromColor color) reflection
    T(texture)

 //checkerboard
let checkerBoard (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
        let xMod = u % factor > factor/2.0
        let yMod = v % factor > factor/2.0
        if (xMod && not yMod) || (not xMod && yMod) then mat1 else mat2
    T(texture)





 


