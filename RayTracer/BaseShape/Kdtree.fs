module Kdtree
open Point
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
        let xdim = (Point.getX max) - (Point.getX min)
        let ydim = (Point.getY max) - (Point.getY min)
        let zdim = (Point.getZ max) - (Point.getZ min) 
        List.maxBy ([xdim;ydim;zdim])

    let rec divide =
