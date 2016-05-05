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
let findMidPoint shapeList = 
    let mutable midPoint = Point.mkPoint 0.0 0.0 0.0
    for triangle in shapeList do
        midPoint <- midPoint + (Shape.getTriangleMidPoint triangle * (1.0 / float(shapeList.Length)))
    midPoint

let rec mkTmKdtree shapes = 
    //Finding biggest dimension in the shapes list

    let axis = snd (BoundingBox.getLongestAxis (mkKdBbox shapes))
    let axisMidPoint = findMidPoint shapes

    //Splitting the shape list in right & left 

    let rec largerThanSplit (xs:Shape list) = 
        match xs with
        |[] -> []
        |x::xs' -> match axis with
                   |"x" -> if Point.getX (Shape.getTriangleMidPoint x) >= Point.getX axisMidPoint then x :: lg xs' else lg xs'
                   |"y" -> if Point.getY (Shape.getTriangleMidPoint x) >= Point.getY axisMidPoint then x :: lg xs' else lg xs' 
                   |"z" -> if Point.getZ (Shape.getTriangleMidPoint x) >= Point.getZ axisMidPoint then x :: lg xs' else lg xs' 
        
    
    let rec lessThanSplit (xs:Shape list) = 
        match xs with
        |[] -> []
        |x::xs' -> match axis with
                   |"x" -> if Point.getX (Shape.getTriangleMidPoint x) <= Point.getX axisMidPoint then x :: lg xs' else lg xs'
                   |"y" -> if Point.getY (Shape.getTriangleMidPoint x) <= Point.getY axisMidPoint then x :: lg xs' else lg xs' 
                   |"z" -> if Point.getZ (Shape.getTriangleMidPoint x) <= Point.getZ axisMidPoint then x :: lg xs' else lg xs' 
         
    //Creating the left and right list from the above 
    let mutable right = largerThanSplit shapes
    let mutable left = lessThanSplit shapes

    if(left.IsEmpty && right.Length > 0) then left <- right
    if(right.IsEmpty && left.Length > 0) then right <- left
    let leftMap = seq { 0 .. count } |> Seq.fold (fun (m: Map<Shape, int>) i -> m.Add(left.[i], 1)) Map.empty
    let countUp =
        match right with
        | [] -> 0
        | x::xs' when (leftMap.ContainsKey x) -> 1 + countUp xs' 
         
    if((countUp/left.Length < 0.5) && countUp/right.Length < 0.5) then 
        let leftTree = (Node(left, Leaf, (mkTmKdtree left),(kdbbox left)) 
        let rightTree = (Node(right, (mkTmKdtree right),Leaf,(kdbbox right)))
        Node(shapes,leftTree, rightTree, kdbbox)
    else Leaf(shapes, kdbbox)

