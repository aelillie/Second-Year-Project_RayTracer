module Texture
open Material
type Texture

///Create a Texture from a function determining how the texture should be mapped
///in respect to the XY axis
val mkTexture : (float -> float -> Material) -> Texture
///Wrap a simple Material into a Texture
val mkMatTexture : mat:Material -> Texture 
///Given a Texture, return the corresponding Material
val getMaterialAtPoint : tex:Texture -> x:float -> y:float -> Material
///Given a function, mapping XY coordinates to a Bitmap XY coordinate set, 
///finds the Material at a specific pixel in the Bitmap and returns a Texture 
val mkTextureFromFile : tr:(float -> float -> float * float) -> file:string -> Texture
