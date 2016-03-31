module Camera

type point = Point.Point
type vector = Vector.Vector

type resolution = 
    | R of int * int

type unitRes =
    | UR of float * float

type camera = 
    | C of point * point * vector * float * unitRes * resolution

type result =           
    | Res of int * int * System.Drawing.Color //TODO make own color implementation.

let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) : camera = C(pos, look, up, zoom, UR(width, height), R(pwidth, pheight))

let shootRays (C(p, q, up, z, (UR(w, h)), (R(pw,ph)))) =
    let l = Point.direction p q
    let r = Vector.crossProduct up l |> Vector.normalise
    let d = Vector.crossProduct r l |> Vector.normalise
    let p' = Point.move p ( z * l)
    let p' = Point.move p' ((h/2.0) * up)
    let p' = Point.move p' ((-w/2.0) * r )
    p'

