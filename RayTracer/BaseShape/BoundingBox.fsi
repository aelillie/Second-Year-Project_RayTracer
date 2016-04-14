module BoundingBox
open Point 
open Shape

type BoundingBox 

val mkBoundingBox : Point -> Point -> BoundingBox 
val calc : Shape -> BoundingBox
val getL : BoundingBox -> Point
val getH : BoundingBox -> Point 