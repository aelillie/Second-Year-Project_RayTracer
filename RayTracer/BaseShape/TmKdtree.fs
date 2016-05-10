module TmKdtree

open Point
open BoundingBox
open Shape

type TmKdtree =
    | Leaf of Shape list  * BoundingBox
    | Node of Shape list * TmKdtree * TmKdtree * BoundingBox  

//Making a boundingbox for the KD-tree, by finding max H point in the boundingboxlist and min l point in the boundingbox list. 
let mkKdBbox shapes =
    //Making boundingbox for all shapes in list
    let sbbox = List.map (fun c -> calc c) shapes
    //Making boundingboxes entire list  
    let max = List.maxBy (fun x -> (Point.getX x,Point.getY x, Point.getZ x)) (List.map (fun b -> getH b) sbbox)
    let min = List.minBy (fun x -> (Point.getX x, Point.getY x, Point.getZ x)) (List.map (fun b -> getL b) sbbox) 
    mkBoundingBox min max 


(* EMIL?? *)
//Finding the midpoint in the triangles in Shapes-list
let rec mkTmKdtree shapes =         
       
     //Finding biggest dimension in the shapes list
    let axis = snd (BoundingBox.getLongestAxis (mkKdBbox shapes))
    let axisMidPoint = 
        let mutable midPoint = Point.mkPoint 0.0 0.0 0.0
        for triangle in shapes do
            midPoint <- midPoint + (Shape.getTriangleMidPoint triangle)
        let avgMid = midPoint / float(shapes.Length)
        avgMid 

    //Splitting the shape list in right & left 
    let rec largerThanSplit (xs:Shape list) = 
        match xs with
        |[] -> []
        |x::xs' -> match axis with
                   |"x" -> if Point.getX (Shape.getTriangleMidPoint x) >= Point.getX axisMidPoint then x :: largerThanSplit xs' else largerThanSplit xs'
                   |"y" -> if Point.getY (Shape.getTriangleMidPoint x) >= Point.getY axisMidPoint then x :: largerThanSplit xs' else largerThanSplit xs' 
                   |"z" -> if Point.getZ (Shape.getTriangleMidPoint x) >= Point.getZ axisMidPoint then x :: largerThanSplit xs' else largerThanSplit xs' 
        
    
    let rec lessThanSplit (xs:Shape list) = 
        match xs with
        |[] -> []
        |x::xs' -> match axis with
                   |"x" -> if Point.getX (Shape.getTriangleMidPoint x) <= Point.getX axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs'
                   |"y" -> if Point.getY (Shape.getTriangleMidPoint x) <= Point.getY axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs' 
                   |"z" -> if Point.getZ (Shape.getTriangleMidPoint x) <= Point.getZ axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs' 
         
    //Creating the left and right list from the above 
    let mutable right = largerThanSplit shapes
    let mutable left = lessThanSplit shapes

    if(left.IsEmpty && right.Length > 0) then left <- right
    if(right.IsEmpty && left.Length > 0) then right <- left

    let leftMap = left |> List.map(fun c -> (c,1)) |> Map.add 

    let leftList = List.map (fun c -> (c,1)) left 

    let countUp =
        match right with
        | [] -> 0
        | x::xs' when (leftMap.ContainsKey x) -> 1 + countUp xs' 

    if((countUp/left.Length < 0.5) && countUp/right.Length < 0.5) then 
      let leftTree = mkTmKdtree left 
      let rightTree = mkTmKdtree right 
      Node(shapes,leftTree, rightTree, (mkKdBbox shapes))
    else Leaf(shapes, (mkKdBbox kdbbox))

