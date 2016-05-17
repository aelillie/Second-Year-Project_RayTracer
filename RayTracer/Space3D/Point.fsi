module Point

type Vector = Vector.Vector
[<Sealed>]
type Point = 
    static member ( + ) : Point * float -> Point
    static member ( - ) : Point * float -> Point
    static member ( + ) : Point * Point -> Point
    static member ( / ) : Point * float -> Point 

val mkPoint : float -> float -> float -> Point
val getX : Point -> float
val getY : Point -> float
val getZ : Point -> float
val getCoord : Point -> float * float * float
val move : Point -> Vector -> Point
val distance : Point -> Point -> Vector
val direction : Point -> Point -> Vector
val round : Point -> int -> Point
val getFromAxis : Point -> string -> float

