module Camera

type point = Point.Point
type vector = Vector.Vector
type ray = Ray.Ray

type Resolution
type UnitRes
type Camera

val mkCamera : point -> point -> vector -> float -> float -> float -> int -> int -> Camera
val mkRays : Camera -> (ray * (int * int)) list