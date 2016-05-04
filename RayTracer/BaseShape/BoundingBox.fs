module BoundingBox
open System
open Point 
open Shape


type BoundingBox = 
    | B of Point * Point 
    override b.ToString() =
        match b with
          B(l,h) -> "("+l.ToString()+","+h.ToString()+")"

let mkBoundingBox l h = B(l,h)

let getH (B(_,h)) = h
let getL (B(l,_)) = l
let getLongestAxis (B(l,h)) =  
    let xdim = ((Point.getX h) - (Point.getX l), "x")
    let ydim = ((Point.getY h) - (Point.getY l), "y")
    let zdim = ((Point.getZ h) - (Point.getZ l), "z") 
    List.maxBy(fun (x,y) -> x) <| [xdim;ydim;zdim]
    

//Epsilon
let epsilon = 0.00001

//Match on shape to calculate max and min values for boundingbox 
let calc s =
    match s with
    | S(p,f,m) -> 
        let lx = (Point.getX p) - f - epsilon
        let ly = (Point.getY p) - f - epsilon
        let lz = (Point.getZ p) - f - epsilon
        let l = mkPoint lx ly lz

        let hx = (Point.getX p) + f + epsilon
        let hy = (Point.getY p) + f + epsilon
        let hz = (Point.getZ p) + f + epsilon 
        let h = mkPoint hx hy hz
        mkBoundingBox l h
    | PL(_,_,_) -> failwith "Can't make boundingbox for that sphere"
    //Triangle bounding box
    | T(a,b,c,m) -> 
        let xlist = [(Point.getX a);(Point.getX b);(Point.getX c)]
        let ylist = [(Point.getY a);(Point.getY b);(Point.getY c)]
        let zlist = [(Point.getZ a);(Point.getZ b);(Point.getY c)]

        let l = Point.mkPoint((List.min xlist) + epsilon) ((List.min ylist)+epsilon) ((List.min zlist)+epsilon)
        let h = Point.mkPoint((List.max xlist) + epsilon) ((List.max ylist)+epsilon) ((List.max zlist)+epsilon)
        B(l,h) 
