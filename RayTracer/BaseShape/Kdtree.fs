module Kdtree
(* open Point
open BoundingBox

type Kdtree =
    | Leaf
    | Node of Kdtree  

let mkKdtree left right shapes =

//Making boundingboxes for all shapes in the Shapes-list
    let sbbox = List.map (fun c -> calc c) shapes

    //Making a boundingbox for the KD-tree, by finding max H point in the boundingboxlist and min l point in the boundingbox list. 
    let kdbbox = 
        let max = List.maxBy (fun x -> (Point.getX x,Point.getY x, Point.getZ x)) (List.map (fun b -> getH b) sbbox)
        let min = List.minBy (fun x -> (Point.getX x, Point.getY x, Point.getZ x)) (List.map (fun b -> getL b) sbbox) 
        let bbox = mkBoundingBox min max  
        //bbox
    //kdbbox

    let maxDimensions =  
        let xdim = ((Point.getX max) - (Point.getX min), "x")
        let ydim = ((Point.getY max) - (Point.getY min), "y")
        let zdim = ((Point.getZ max) - (Point.getZ min), "z") 
        List.item 0 ( List.maxBy((fun (x,y) -> x) <| ([xdim;ydim;zdim]))

    let root = 

    

    let mutable midpoint = Point.mkPoint 0 0 0
    let rec mkmidpoint lshape =
        match lshape with   
        | S(o,r,mat) -> 
            let midpoint = Point.mkPoint (Point.getX o) (Point.getY o) (Point.getZ o)
            midpoint  
        | PL(mat,pVector,n) -> Point.mkPoint 0 0 0 
        | T(a,b,c,mat) -> 
            let midpoint = 
                let x = ((Point.getX a) + (Point.getX b) + (Point.getX c))/3
                let y = ((Point.getY a) + (Point.getY b) + (Point.getY c))/3
                let z = ((Point.getZ a) + (Point.getZ b) + (Point.getZ c))/3
                Point.mkPoint x y z
            midpoint 
        | TShape (s, tr) -> 
            mkmidpoint s 
        | 

        *)