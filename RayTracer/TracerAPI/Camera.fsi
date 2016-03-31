module Camera

type point = Point.Point
type vector = Vector.Vector

type camera

val mkCamera : point -> point -> vector -> float -> float -> float -> float -> float