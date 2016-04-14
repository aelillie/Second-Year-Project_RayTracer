module BoundingBox
open System
open Point 
open Shape
open Vector 

type BoundingBox = 
    | B of Point * Point 

let mkBoundingBox l h = B(l,h)

let calcL s =
    match s with
    | S(p,f,m) -> 
        let lx = (Point.getX p) - f 
        let ly = (Point.getY p) - f 
        let lz = (Point.getZ p) - f
        let l = P(lx,ly,lz) 

        let hx = (Point.getX p) + f 
        let hy = (Point.getY p) + f 
        let hz = (Point.getZ p) + f
        let h = P(hx,hy,hz)
        B(l,h) 

