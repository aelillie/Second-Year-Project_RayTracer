module BoundingBox
open Point 
open Shape

type BoundingBox 

val mkBoundingBox : Point -> Point -> BoundingBox 
val calc : Shape -> BoundingBox
val getH : BoundingBox -> Point
val getL : BoundingBox -> Point

 