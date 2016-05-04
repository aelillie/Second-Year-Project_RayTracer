module TmKdtree

open Point
open BoundingBox
open Shape

type TmKdtree =
    | Leaf of TmKdtree
    | Node of Shape list * TmKdtree * TmKdtree * BoundingBox  


let rec mkTmKdtree shapes = 
    //Making boundingboxes for all triangles in the Shapes-list
    let sbbox = List.map (fun c -> calc c) shapes

    //Making a boundingbox for the KD-tree, by finding max H point in the boundingboxlist and min l point in the boundingbox list. 
    let kdbbox = 
        let max = List.maxBy (fun x -> (Point.getX x,Point.getY x, Point.getZ x)) (List.map (fun b -> getH b) sbbox)
        let min = List.minBy (fun x -> (Point.getX x, Point.getY x, Point.getZ x)) (List.map (fun b -> getL b) sbbox) 
        mkBoundingBox min max  

    let axis = snd (BoundingBox.getLongestAxis kdbbox)
    let axisLength = fst (BoundingBox.getLongestAxis kdbbox)

    //Finding the midpoint in the triangles in Shapes-list
    (* let findMidPoint (shapeList:Shape List) = 
        let mutable midPoint = Point.mkPoint 0.0 0.0 0.0
        for triangle in shapeList do
            midPoint <- midPoint + (Shape.getTriangleMidPoint triangle * (1.0 / (shapeList.Length |> float))
        midPoint *)

    let largerThanSplit shapes =
        for triangle in shapes do
            match axis with
            |"x" -> if Point.getX (Shape.getTriangleMidPoint triangle) >= axisLength then triangle 
            |"y" -> if Point.getY (Shape.getTriangleMidPoint triangle) >= axisLength then triangle 
            |"z" -> if Point.getZ (Shape.getTriangleMidPoint triangle) >= axisLength then triangle
    let mutable right = List.map(fun c -> largerThanSplit c) shapes


    let lessThanSplit shapes =
        for triangle in shapes do
            match axis with
            |"x" -> if Point.getX (Shape.getTriangleMidPoint triangle) <= axisLength then triangle 
            |"y" -> if Point.getY (Shape.getTriangleMidPoint triangle) <= axisLength then triangle 
            |"z" -> if Point.getZ (Shape.getTriangleMidPoint triangle) <= axisLength then triangle
    let mutable left = List.map(fun c -> lessThanSplit c) shapes

    let check = 
        if(left.IsEmpty && right.Length > 0) then left <- right
        if(right.IsEmpty && left.Length > 0) then right <- left
        let mutable counter = 0
        let hashleft = List.map (fun c -> Map.add c 1 hashLeft) left 
        let countUp = List.map(fun c -> if(Map.containsKey c) then counter <- counter + 1) right
        if(counter > 50) then 
            let leftTree = (Node(left, Leaf, (mkTmKdtree left),kdbbox)) 
            let rightTree = (Node(right, (mkTmKdtree right),Leaf,kdbbox)
        else Leaf 

