module Texture
open Material
open System.Drawing

type Texture =
    | T of (float -> float -> Material)


 let mkTexture (f : float -> float -> Material) : Texture = T(f)

 let mkMatTexture (m : Material) : Texture = mkTexture (fun x y -> m)

 let getMaterialAtPoint (T(f)) x y = f x y

 //chekcerboard texture
 let checkerBoard (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
   //let factor = 1.0
        let xMod = u % factor > (factor/2.0)
        let yMod = v % factor > (factor/2.0)
        if (xMod && not yMod) || (not xMod && yMod) then mat1 else mat2
    T(texture)


//nice test texture
let testTexture = 
    let texture (u:float) (v:float) : Material = mkMaterial(Colour.mkColour u v 0.0) 0.0
    T(texture)






 //stribes as texture that magically happend when I tried to fix checkerboard
let stribes (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
        let xMod = u % factor > factor/2.0
        let yMod = v % factor > factor/2.0
        if (xMod && not yMod) || (not xMod && not yMod) then mat1 else mat2
    T(texture)





 


