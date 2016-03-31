module Camera

type point = Point.Point
type vector = Vector.Vector

type resolution = 
    | R of int * int

type unitRes =
    | UR of float * float

type camera = 
    | C of point * point * vector * float * unitRes * resolution

let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) : camera = C(pos, look, up, zoom, UR(width, height), R(pwidth, pheight))