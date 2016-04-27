module Texture
open Material
type Texture =
    | T of (float -> float -> Material)


 let mkTexture (f : float -> float -> Material) : Texture = T(f)

 let mkMatTexture (m : Material) : Texture = mkTexture (fun x y -> m)

 let getMaterialAtPoint (T(f)) x y = f x y

 let checkerBoard (mat1:Material) (mat2:Material) (factor:float) = 
    let texture (u:float) (v:float) = 
        let factor = 0.5
        let xMod = u % factor
        let yMod = v % factor
        if abs(xMod) > (factor/2.0) then
            if abs(yMod) < (factor/2.0) then mat1 else mat2
        else
            if abs(yMod) < (factor/2.0) then mat2 else mat1
    T(texture)

