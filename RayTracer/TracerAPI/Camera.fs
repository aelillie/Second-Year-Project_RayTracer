module Camera
open System.Threading.Tasks

type point = Point.Point
type vector = Vector.Vector
type ray = Ray.Ray

type resolution = 
    | Res of int * int

type unitRes =
    | UR of float * float

type camera = 
    | C of point * point * vector * float * unitRes * resolution

type result =           
    | RayRes of int * int * System.Drawing.Color //TODO make own color implementation.

let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) : camera = C(pos, look, up, zoom, UR(width, height), Res(pwidth, pheight))

let mkRays (C(p, q, up, z, (UR(w, h)), (Res(pw,ph)))) =
    let l = Point.direction p q
    let r = Vector.crossProduct l up |> Vector.normalise
    let d = Vector.crossProduct l r |> Vector.normalise
    let p' = Point.move p ( z * l)
    let p' = Point.move p' ((h/2.0) * up)
    let p' = Point.move p' ((w/2.0) * r )
    let W = w/float pw
    let H = h/float ph

    let createRay (a, b) p1 p2 = 
        let p2 = Point.move p2 (((float a + 0.5) * W) * r)
        let p2 = Point.move p2 (((float b + 0.5) * H) * d)
        let v = Point.direction p1 p2
        Ray.mkRay a b p2 1.0 v

    [for x in 0..pw do
         for y in 0..ph do           
            yield createRay (x,y) p p']
//    let result = List.init (pw * ph)
//
//    Parallel.For (0,pw, (fun x -> for y in 0..ph do result.[(y*x)] <- createRay (x,y) p p')) |> ignore
    


