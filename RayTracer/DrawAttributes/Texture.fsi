module Texture
open Material
type Texture

val mkTexture : (float -> float -> Material) -> Texture
val mkMatTexture : mat:Material -> Texture 
val getMaterialAtPoint : tex:Texture -> x:float -> y:float -> Material
val checkerBoard : mat1:Material -> mat2:Material -> factor:float -> Texture
val testTexture : Texture

