module BoundingBox
open System
open Point 
open Shape
open Vector 

type BoundingBox = 
    | B of Point * Point 

let mkBoundingBox l h = B(l,h)

let epsilon = 0.00001

let calcL s =
    match s with
    | S(p,f,m) -> 
        let lx = (Point.getX p) - f - epsilon
        let ly = (Point.getY p) - f - epsilon
        let lz = (Point.getZ p) - f - epsilon
        let l = P(lx,ly,lz) 

        let hx = (Point.getX p) + f + epsilon
        let hy = (Point.getY p) + f + epsilon
        let hz = (Point.getZ p) + f + epsilon 
        let h = P(hx,hy,hz)
        B(l,h)
    | S(a,b,c,m) -> 
        let xlist = [(Point.getX a);(Point.getX b);(Point.getX c)]
        let ylist = [(Point.getY a);(Point.getY b);(Point.getY c)]
        let zlist = [(Point.getZ a);(Point.getZ b);(Point.getY c)]

        let l = P((List.min xlist), (List.min ylist), (List.min zlist))
        let h = P((List.max xlist), (List.max ylist), (List.max zlist))
        B(l,h)