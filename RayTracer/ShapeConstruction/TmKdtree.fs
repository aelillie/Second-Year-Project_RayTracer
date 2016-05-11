module TmKdtree

open Point
open Shapes

type BoundingBox = BasicShape.BoundingBox 
type Shape = BasicShape.Shape

type TmKdtree =
    | Leaf of BasicShape.Triangle list * BoundingBox 
    | Node of BasicShape.Triangle list * TmKdtree * TmKdtree * BoundingBox  

//Making a boundingbox for the KD-tree, by finding max H point in the boundingboxlist and min l point in the boundingbox list. 
let mkKdBbox (shapes : BasicShape.Triangle list) : BoundingBox =
    let shapeX = List.map(fun x -> x:> Shape) shapes
    let sbbox = List.map (fun (c:Shape) -> c.getBounding()) shapeX
    let max = List.maxBy (fun x -> (Point.getX x,Point.getY x, Point.getZ x)) (List.map (fun (b:BasicShape.BoundingBox) -> b.getH) sbbox)
    let min = List.minBy (fun x -> (Point.getX x, Point.getY x, Point.getZ x)) (List.map (fun (b:BasicShape.BoundingBox) -> b.getL) sbbox) 
    {p1=min ; p2=max} 

let getShapes(Node(b,_,_,_)) = b
let getBox (Node(_,_,_,bbox)) = bbox


    
    
//Finding the midpoint in the triangles in Shapes-list
let rec mkTmKdtree (shapes : BasicShape.Triangle list) =               
     //Finding biggest dimension in the shapes list
    let box = mkKdBbox shapes
    let axis = snd (box.getLongestAxis box.getL box.getH)
    let axisMidPoint = 
        let mutable midPoint = Point.mkPoint 0.0 0.0 0.0
        for (triangle:BasicShape.Triangle) in shapes do
            midPoint <- midPoint + (triangle.getMidPoint())
        let avgMid = midPoint / float(shapes.Length)
        avgMid 

    //Splitting the shape list in right & left 
    let rec largerThanSplit (xs:BasicShape.Triangle list) = 
        match xs with
        |[] -> []
        |x::xs' -> match axis with
                   |"x" -> if Point.getX (x.getMidPoint()) >= Point.getX axisMidPoint then x :: largerThanSplit xs' else largerThanSplit xs'
                   |"y" -> if Point.getY (x.getMidPoint()) >= Point.getY axisMidPoint then x :: largerThanSplit xs' else largerThanSplit xs' 
                   |"z" -> if Point.getZ (x.getMidPoint()) >= Point.getZ axisMidPoint then x :: largerThanSplit xs' else largerThanSplit xs' 
        
    
    let rec lessThanSplit (xs:BasicShape.Triangle list) = 
        match xs with
        |[] -> []
        |x::xs' -> match axis with
                   |"x" -> if Point.getX (x.getMidPoint()) <= Point.getX axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs'
                   |"y" -> if Point.getY (x.getMidPoint()) <= Point.getY axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs' 
                   |"z" -> if Point.getZ (x.getMidPoint()) <= Point.getZ axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs' 
         
    //Creating the left and right list from the above 
    let mutable right = largerThanSplit shapes
    let mutable left = lessThanSplit shapes

    if(left.IsEmpty && right.Length > 0) then left <- right
    if(right.IsEmpty && left.Length > 0) then right <- left

    let mutable count = 0 
    let leftMap = 
        for t in left do
            for k in right do 
                if(t = k) then count <- count + 1 

    if((float(count/left.Length) < 0.5) && float(count/right.Length) < 0.5) then 
      let leftTree = mkTmKdtree left 
      let rightTree = mkTmKdtree right 
      Node(shapes,leftTree, rightTree, (mkKdBbox shapes))
    else Leaf(shapes, (mkKdBbox shapes))
