module Texture
open Material
type Texture =
    | T of float * float

let mkTexture (x:float) (y:float) (mat:Material) : Texture = failwith "mkTexture not implemented"
let mkMatTexture (mat:Material) : Texture = failwith "mkMatTexture not implemented"