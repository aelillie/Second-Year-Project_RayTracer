module Camera

type point = Point.Point
type vector = Vector.Vector
type ray = Ray.Ray

type resolution
type unitRes
type camera
type result


val mkCamera : point -> point -> vector -> float -> float -> float -> int -> int -> camera

val mkRays : camera -> (ray * (int * int)) list