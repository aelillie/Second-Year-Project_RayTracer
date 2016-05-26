module Camera

type point = Point.Point
type vector = Vector.Vector
type ray = Ray.Ray

///Actual pixel resolution of the drawing
type Resolution
///A pixel's unit resolution
type UnitRes
///Representation of a camera, made up by its position, entry view point on the view plane,
///direction vector, zoom scale, unit resolution and drawing resolution of the scene
type Camera

val mkCamera : point -> point -> vector -> float -> float -> float -> int -> int -> Camera

val getRes : Camera -> int * int
//Takes a camera and creates all the ray from it coupled together with the corresponding xy coordinat.
val mkRays : Camera -> (ray * (int * int)) list

val getPoint : Camera -> point