﻿module Camera
//open System.Threading.Tasks

type point = Point.Point
type vector = Vector.Vector
type ray = Ray.Ray

///Actual pixel resolution of the drawing
type Resolution = 
    | Res of int * int

///A pixel's unit resolution
type UnitRes =
    | UR of float * float

///Representation of a camera, made up by its position, entry view point on the view plane,
///direction vector, zoom scale, unit resolution and drawing resolution of the scene
type Camera = 
    | C of point * point * vector * float * UnitRes * Resolution


let mkCamera pos look up zoom width height pwidth pheight = 
    C(pos, look, up, zoom, UR(width, height), Res(pwidth, pheight))

let getRes (C(p, q, up, z, (UR(w, h)), (Res(pw,ph)))) = (pw,ph)

let mkRays (C(p, q, up, z, (UR(w, h)), (Res(pw,ph)))) =
    let l = Point.direction p q //normalised direction vector
    let r = Vector.crossProduct l up //right direction vector
            |> Vector.normalise
    let d = -up //down direction vector
            |> Vector.normalise
    //Move position to the top left corner of view plane
    let p' = Point.move p ( z * l) //to the center
    let p' = Point.move p' ((h/2.0) * up) //to the top
    let p' = Point.move p' ((-w/2.0) * r ) //to the left
    let (W, H) = //Resolution of each pixel in units
        (w/float pw, h/float ph)

    let createRay (xx, yy) p1 p2 = 
        let p2 = //move p' to pixel column
            Point.move p2 (((float xx + 0.5) * W) * r)
        let p2 = //move p' to pixel row
            Point.move p2 (((float yy + 0.5) * H) * d)
        let v = Point.direction p1 p2 //ray direction
        Ray.mkRay p2 v
       
    [for x in 0..pw do
         for y in 0..ph do           
            yield createRay (x,y) p p', (x,y)]
//    let result = List.init (pw * ph)
//
//    Parallel.For (0,pw, (fun x -> for y in 0..ph do result.[(y*x)] <- createRay (x,y) p p')) |> ignore
    
let getPoint (C(p,_,_,_,_,_)) = p

