module Texture
open Material
open System.Drawing

type Texture =
    | T of (float -> float -> Material)


 let mkTexture (f : float -> float -> Material) : Texture = T(f)

 let mkMatTexture (m : Material) : Texture = mkTexture (fun x y -> m)

 let getMaterialAtPoint (T(f)) x y = f x y

 //chekcerboard texture 
 (*
 let checkerBoard (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
   //let factor = 1.0
        let xMod = u % factor > (factor/2.0)
        let yMod = v % factor > (factor/2.0)
        if (xMod && not yMod) || (not xMod && yMod) then mat1 else mat2
    T(texture)
   


let checkerBoard (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
        let xMod = u % factor
        let yMod = v % factor
        if xMod > 2.0 then mat1 
            elif yMod < 2.0 then mat1 else mat2
    T(texture)
    *)

//nice test texture
let testTexture = 
    let texture (u:float) (v:float) : Material = Material.mkMaterial (Colour.mkColour u v 10.0) 0.0
    T(texture)

//loading a texture
let loadTexture (file : string) =
 let img = new Bitmap(file) 
 let widthf = float (img.Width - 1)
 let heightf = float (img.Height - 1)
 let texture (u : float) (v : float) = 
   //  evt lock? lock img (fun _ -> 
            let imgColor = img.GetPixel (int (widthf * (u%1.0)), int (heightf * (1.0 - (v%1.0))))
            let colour = Colour.mkColour ((float) imgColor.R/255.0) ((float) imgColor.G/255.0) ((float) imgColor.B/255.0)
            mkMaterial colour 0.0
 T(texture)
    

    //(int (u * (v - 1.0)), int ((1.0 - v) * (heightf - 1.0)))

 (*let loadTexture (file : string) =
 let img = new Bitmap(file) 
 let widthf = float (img.Width - 1)
 let heightf = float (img.Height - 1)
 let texture (u : float) (v : float) = 
     let imgColor = img.GetPixel (int (widthf * (u%1.0)), int (heightf * (1.0 - (v%1.0))))
  //   let c1 = Colour.mkColour ((float) imgColor.R) ((float) imgColor.G) ((float) imgColor.B)
//     let color = Colour.fromColor imgColor
    // let c = Colour.mkColour (Colour.toColor (((float) imgColor.R) ((float) imgColor.G) ((float) imgColor.B)))
     mkMaterial (Colour.fromColor imgColor) 0.0
 T(texture)
*)

 //checkerboard
let checkerBoard (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
        let xMod = u % factor > factor/2.0
        let yMod = v % factor > factor/2.0
        if (xMod && not yMod) || (not xMod && yMod) then mat1 else mat2
    T(texture)





 


