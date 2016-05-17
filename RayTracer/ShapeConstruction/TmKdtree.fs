﻿module TmKdtree

open Point
open Shapes

type BoundingBox = BasicShape.BoundingBox 
type Shape = BasicShape.Shape

type TmKdtree =
    | Leaf of BasicShape.Triangle list * BoundingBox 
    | Node of BasicShape.Triangle list * TmKdtree * TmKdtree * BoundingBox  * (string*Point)

//Making a boundingbox for the KD-tree, by finding max H point in the boundingboxlist and min l point in the boundingbox list. 
let mkKdBbox (shapes : BasicShape.Triangle list) : BoundingBox =
    let epsilon = 0.00001
    let shapeX = List.map(fun x -> x:> Shape) shapes
    let sbbox = List.map (fun (c:Shape) -> c.getBounding().Value) shapeX
    let bL = List.map (fun (b:BasicShape.BoundingBox) -> b.getL) sbbox
    let bH = List.map (fun (b:BasicShape.BoundingBox) -> b.getH) sbbox

    let minX = List.minBy (fun x -> Point.getX x) bL
    let minY = List.minBy (fun x -> Point.getY x) bL
    let minZ = List.minBy (fun x -> Point.getZ x) bL

    let maxX = List.maxBy (fun x -> Point.getX x) bH
    let maxY = List.maxBy (fun x -> Point.getY x) bH
    let maxZ = List.maxBy (fun x -> Point.getZ x) bH
    {p1=(mkPoint (Point.getX minX - epsilon) (Point.getY minY - epsilon) (Point.getZ minZ - epsilon) ) 
                                     ; p2=(mkPoint (Point.getX maxX + epsilon) (Point.getY maxY + epsilon) (Point.getZ maxZ + epsilon) )}
    

//Get left node
let getLeft s = 
    match s with
    | Node(_,l,_,_,_) -> l

let getRight s = 
    match s with
    | Node(_,_,r,_,_) -> r



//Get the triangle list
let getShapes s = 
    match s with
    | Node(b,_,_,_,_) -> b
    | Leaf(b,_) -> b

let getAxis s =
    match s with
    | Node(_,_,_,_,a) -> a


//Get bounding box
let getBox s =
    match s with
    | Node(_,_,_,b,_) -> Some b
    | Leaf(_,b) -> Some b

let closestHit (triList : BasicShape.Triangle list) ray =
    let sndRects = List.map(fun x -> x:> Shape) triList
    let min = List.map(fun (x:Shape) -> x.hit ray) sndRects |> List.choose id
    match min with
    |[] -> None
    |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)

let searchLeaf leaf ray t' =
    match leaf with 
    | Leaf(s,_) -> let hit = closestHit s ray
                   match hit with
                   |Some(f,_,_) when f<t' -> Some hit
                   |Some(f,_,_) when f>t' -> None
                   |None -> None

let order(d, left, right) =
    if d > 0.0
    then (left, right)
    else (right, left)

let rec search node ray t t' =
    match node with
    |Leaf(_,_) -> searchLeaf node ray t'
    |Node(_,_,_,_,a') -> 
        let a = fst a'
        let b = snd a'
        if(Ray.getDirection ray a) = 0.0 then
            if((Ray.getOrigin ray a) <= (Point.getFromAxis b a)) then search (getLeft node) ray t t'
            else search (getRight node) ray t t' 
        else 
            let tHit = ((Point.getFromAxis b a) - (Ray.getOrigin ray a)) / Ray.getDirection ray a
            let (fst, snd) = order((Ray.getDirection ray a),getLeft node, getRight node)
            if tHit >= t || tHit < 0.0 then
                search fst ray t t'
            else if tHit <= t then
                search snd ray t t'
            else
             match search fst ray t tHit with
             |Some hit -> Some hit
             | _ -> search snd ray tHit t'


let traverse tree ray =
    match(getBox tree).Value.hit(ray) with
    |Some(t,t') -> search tree ray t t'
    |None -> None
    
//Finding the midpoint in the triangles in Shapes-list - we do this (recursively) to find out what axis to split 
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

    //If one of the trees are empty, we add make left and right equivelant. 
    if(left.IsEmpty && right.Length > 0) then left <- right
    if(right.IsEmpty && left.Length > 0) then right <- left

    (*
    let count = 0
    let checking = List.fold (List.fold *)
      

    let mutable count = 0 
    let leftMap = 
        for t in left do
            for k in right do 
                if(t = k) then count <- count + 1 
                

    if((float(count/left.Length) < 0.5) && float(count/right.Length) < 0.5) then 
      let leftTree = mkTmKdtree left 
      let rightTree = mkTmKdtree right 
      Node(List.empty,leftTree, rightTree, (mkKdBbox shapes),(axis,axisMidPoint))
    else Leaf(shapes, (mkKdBbox shapes))
