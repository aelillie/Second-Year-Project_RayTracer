module Camera

type point = Point.Point
type vector = Vector.Vector

type resolution
type unitRes
type camera
type result


val mkCamera : point -> point -> vector -> float -> float -> float -> int -> int -> camera

val shootRays : camera -> point