module Texture
open Material
type Texture

val mkTexture : x:float -> y:float -> mat:Material -> Texture
val mkMatTexture : mat:Material -> Texture
