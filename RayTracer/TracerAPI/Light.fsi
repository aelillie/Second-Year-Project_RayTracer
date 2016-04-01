module Light
open Point
open Material

type Light
type AmbientLight

val mkLight : position : Point -> colour : Colour -> intensity : float -> Light
val mkAmbientLight : colour : Colour -> intensity : float -> AmbientLight
