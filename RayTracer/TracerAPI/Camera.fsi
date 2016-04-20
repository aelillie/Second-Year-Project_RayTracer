module Camera

type point = Point.Point
type vector = Vector.Vector
type ray = Ray.Ray

type Resolution
type UnitRes
type Camera

val mkCamera : point -> point -> vector -> float -> float -> float -> int -> int -> Camera

val getRes : Camera -> int * int
//Takes a camera and creates all the ray from it coupled together with the corresponding xy coordinat.
val mkRays : Camera -> (ray * (int * int)) list

val getPoint : Camera -> point