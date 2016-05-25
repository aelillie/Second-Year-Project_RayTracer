module TmKdtree

open Point
open Shapes

type BoundingBox = BasicShape.BoundingBox 
type Shape = BasicShape.Shape

type TmKdtree =
    | Leaf of BasicShape.Triangle list * BoundingBox 
    | Node of BasicShape.Triangle list * TmKdtree * TmKdtree * BoundingBox  * (string*Point)

let epsilon = 0.00001

//Making a boundingbox for the KD-tree, by finding max H point in the boundingboxlist and min l point in the boundingbox list. 
let mkKdBbox (shapes : BasicShape.Triangle list) : BoundingBox =
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
    {p1=(mkPoint (Point.getX minX) (Point.getY minY) (Point.getZ minZ) ) 
                                     ; p2=(mkPoint (Point.getX maxX) (Point.getY maxY) (Point.getZ maxZ) )}
    
//Splitting existing boundingbox according to left and right list of shapes
let BoundingBoxL (bbox:BoundingBox) axis midp : BoundingBox = 
    match axis with
    | "x" -> {p1 = bbox.getL; p2 = Point.mkPoint ((Point.getX midp)) ((Point.getY (bbox.getH))) ((Point.getZ (bbox.getH)))}
    | "y" -> {p1 = bbox.getL; p2 = Point.mkPoint (Point.getX (bbox.getH)) ((Point.getY midp)) ((Point.getZ (bbox.getH)))}
    | "z" -> {p1 = bbox.getL; p2 = Point.mkPoint (Point.getX (bbox.getH)) (Point.getY (bbox.getH)) (Point.getZ midp)}
    | _ -> failwith "Unknown axis"


let BoundingBoxR (bbox:BoundingBox) axis midp : BoundingBox = 
    match axis with
    | "x" -> {p1 = (Point.mkPoint (Point.getX midp) (Point.getY (bbox.getL)) (Point.getZ (bbox.getL))); p2 = bbox.getH}
    | "y" -> {p1 = (Point.mkPoint (Point.getX (bbox.getL)) (Point.getY midp) (Point.getZ (bbox.getL))); p2 = bbox.getH}
    | "z" -> {p1 = (Point.mkPoint (Point.getX (bbox.getL)) (Point.getY (bbox.getL)) (Point.getZ midp)); p2 = bbox.getH}
    | _ -> failwith "Unknown axis"


//Get left node
let getLeft s = 
    match s with
    | Node(_,l,_,_,_) -> l
    | Leaf(_,_) as leaf -> leaf 

let getRight s = 
    match s with
    | Node(_,_,r,_,_) -> r
    | Leaf(_,_) as leaf -> leaf


//Get the triangle list
let getShapes s = 
    match s with
    | Node(b,_,_,_,_) -> b
    | Leaf(b,_) -> b

let getAxis s =
    match s with
    | Node(_,_,_,_,a) -> a
    | Leaf(_,_) -> failwith "leaf ramt af axis"


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
                   |Some(f,_,_) -> if (f<t') then Some hit else None
                   |None -> None
    | Node(_,_,_,_,_) -> failwith "Expected leaf"

let order(d, left, right) =
    if d > 0.0
    then (left, right)
    else (right, left)

let rec search node ray t t' =
    match node with
    |Leaf(_,_) -> searchLeaf node ray t'
    |Node(_,_,_,_,a') -> 
        let direction = Ray.getDirection ray (fst a')
        let origin = Ray.getOrigin ray (fst a')
        let nodeValue = Point.getFromAxis (snd a') (fst a')
        if(direction) = 0.0 then
            if(origin <= nodeValue) then search (getLeft node) ray t t'
            else search (getRight node) ray t t' 
        else 
            let tHit = (nodeValue - origin) / direction
            let (fst, snd) = order(direction,getLeft node, getRight node)
            if tHit <= t || tHit < 0.0 then
                search snd ray t t'
            else if tHit >= t' then
                search fst ray t t'
            else
             match search fst ray t tHit with
             |Some hit -> Some hit
             |_ -> search snd ray tHit t'


let traverse tree ray (bawx:BasicShape.BoundingBox) =
    match(bawx).hit(ray) with
    |Some(t,t') ->  search tree ray t t'  
    |None -> None


//Finding the midpoint in the triangles in Shapes-list - we do this (recursively) to find out what axis to split 
let rec mkTmKdtree (shapes : BasicShape.Triangle list) (box:BasicShape.BoundingBox) =               
     //Finding biggest dimension in the shapes list
    let axis = snd (box.getLongestAxis)
    let axisMidPoint = 
        let midPoint = List.fold (fun acc (ele:BasicShape.Triangle) -> (acc + ele.getMidPoint())) (Point.mkPoint 0.0 0.0 0.0) shapes
        let avgMid = midPoint / float(shapes.Length)
        avgMid 
    


    //Splitting the shape list in right & left 
    let rec largerThanSplit (xs:BasicShape.Triangle list) = 
        let results = List.choose(fun (elem:BasicShape.Triangle) ->
                        match axis with
                        |"x" -> let (x1,x2,x3) = elem.getCoords "x"
                                let mpX = (Point.getX axisMidPoint)
                                if x1>=mpX || x2>=mpX ||x3>=mpX  then Some elem else None
                        |"y" -> let (y1,y2,y3) = elem.getCoords "y"
                                let mpY = (Point.getY axisMidPoint)
                                if y1>=mpY || y2>=mpY ||y3>=mpY  then Some elem else None
                        |"z" -> let (z1,z2,z3) = elem.getCoords "z"
                                let mpZ = (Point.getZ axisMidPoint)
                                if z1>=mpZ || z2>=mpZ ||z3>=mpZ  then Some elem else None
                        | _ -> failwith "Unknown axis") xs
        results


    let rec lessThanSplit (xs:BasicShape.Triangle list) = 
        let results = List.choose(fun (elem:BasicShape.Triangle) ->
                        match axis with
                        |"x" -> let (x1,x2,x3) = elem.getCoords "x"
                                let mpX = (Point.getX axisMidPoint)
                                if x1<=mpX || x2<=mpX ||x3<=mpX  then Some elem else None
                        |"y" -> let (y1,y2,y3) = elem.getCoords "y"
                                let mpY = (Point.getY axisMidPoint)
                                if y1<=mpY || y2<=mpY ||y3<=mpY  then Some elem else None
                        |"z" -> let (z1,z2,z3) = elem.getCoords "z"
                                let mpZ = (Point.getZ axisMidPoint)
                                if z1<=mpZ || z2<=mpZ ||z3<=mpZ  then Some elem else None
                        | _ ->  None) xs
        results
         
    //Creating the left and right list from the above 
    let right = largerThanSplit shapes
    let left = lessThanSplit shapes

    //If one of the trees are empty, we add make left and right equivelant. 
    //let left = if(leftTest.IsEmpty && rightTest.Length > 0) then rightTest else leftTest
   // let right = if(rightTest.IsEmpty && leftTest.Length > 0) then leftTest else rightTest

    //Check for duplicates among the lists. 
    if(((float(left.Length+right.Length)/float(shapes.Length)) < 1.6) && left.Length <> shapes.Length && right.Length<>shapes.Length) then 
      let leftTree = mkTmKdtree left (BoundingBoxL box axis axisMidPoint)
      let rightTree = mkTmKdtree right (BoundingBoxR box axis axisMidPoint)
      Node(shapes,leftTree, rightTree, (box),(axis,axisMidPoint))

    else Leaf(shapes, (box))

