module Texture
open Material
type Texture

val mkTexture : (float -> float -> Material) -> Texture
val mkMatTexture : mat:Material -> Texture 
val getMaterialAtPoint : tex:Texture -> x:float -> y:float -> Material
val mkTextureFromFile : tr:(float -> float -> float * float) -> file:string -> Texture
