module TmKdtree

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
            printfn("%s") "flatsite"
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
    |Some(t,t') ->  search tree ray t t'  
    |None -> None
    
//Finding the midpoint in the triangles in Shapes-list - we do this (recursively) to find out what axis to split 
let rec mkTmKdtree (shapes : BasicShape.Triangle list) =               
     //Finding biggest dimension in the shapes list
    let box = mkKdBbox shapes
    let axis = snd (box.getLongestAxis)
    let axisMidPoint = 
        let midPoint = List.fold (fun acc (ele:BasicShape.Triangle) -> (acc + ele.getMidPoint())) (Point.mkPoint 0.0 0.0 0.0) shapes
        let avgMid = midPoint / float(shapes.Length)
        avgMid 

    //Splitting the shape list in right & left 
    let rec largerThanSplit (xs:BasicShape.Triangle list) = 
        let results = List.choose(fun (elem:BasicShape.Triangle) ->
            match axis with
            |"x" -> let (x1,x2,x3) = elem.getXCoords()
                    let mpX = (Point.getX axisMidPoint)
                    if x1>=mpX || x2>=mpX ||x3>=mpX  then Some elem else None
            |"y" -> let (y1,y2,y3) = elem.getYCoords()
                    let mpY = (Point.getY axisMidPoint)
                    if y1>=mpY || y2>=mpY ||y3>=mpY  then Some elem else None
            |"z" -> let (z1,z2,z3) = elem.getZCoords()
                    let mpZ = (Point.getZ axisMidPoint)
                    if z1>=mpZ || z2>=mpZ ||z3>=mpZ  then Some elem else None) xs
        results
    let rec lessThanSplit (xs:BasicShape.Triangle list) = 
        let results = List.choose(fun (elem:BasicShape.Triangle) ->
            match axis with
            |"x" -> let (x1,x2,x3) = elem.getXCoords()
                    let mpX = (Point.getX axisMidPoint)
                    if x1<=mpX || x2<=mpX ||x3<=mpX  then Some elem else None
            |"y" -> let (y1,y2,y3) = elem.getYCoords()
                    let mpY = (Point.getY axisMidPoint)
                    if y1<=mpY || y2<=mpY ||y3<=mpY  then Some elem else None
            |"z" -> let (z1,z2,z3) = elem.getZCoords()
                    let mpZ = (Point.getZ axisMidPoint)
                    if z1<=mpZ || z2<=mpZ ||z3<=mpZ  then Some elem else None) xs
        results
    (*let rec largerThanSplit (xs:BasicShape.Triangle list) = 
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
                   |"z" -> if Point.getZ (x.getMidPoint()) <= Point.getZ axisMidPoint then x :: lessThanSplit xs' else lessThanSplit xs' *)
         
    //Creating the left and right list from the above 
    let rightTest = largerThanSplit shapes
    let leftTest = lessThanSplit shapes

    //If one of the trees are empty, we add make left and right equivelant. 
    let left = if(leftTest.IsEmpty && rightTest.Length > 0) then rightTest else leftTest
    let right = if(rightTest.IsEmpty && leftTest.Length > 0) then leftTest else rightTest

   (* let rec findMatches acc one two =
        match one with
        |x::xs ->   match two with
                    |y::ys -> if(x=y) then 
                                    findMatches (acc+1) one ys
                                else findMatches acc one ys
                    |[] -> findMatches acc xs two
        |[] -> acc 
        *)

    
    printfn("%s %i %i") "SAMMENLIGN" left.Length right.Length
    (*let mutable count2 = 0
    let findmatches2 =   
            for t in left do
                for k in right do 
                    if(t = k) then count2 <- count2 + 1   
    printfn("%s %i %i") "Result" left.Length right.Length*)
    //let count = findMatches 0 left right
    let boolbi = if((left.Length+right.Length)/shapes.Length

    if((float(count2/left.Length) < 0.5) && float(count2/right.Length) < 0.5) then 
      //printfn("%s") "jeg har lavet et node din fucking lort"
      //Node(List.empty, (mkTmKdtree left), (mkTmKdtree right), (mkKdBbox shapes), (axis,axisMidPoint))
      let leftTree = mkTmKdtree left 
      let rightTree = mkTmKdtree right 
      printfn("%s") "NODE"
      Node(List.empty,leftTree, rightTree, (mkKdBbox shapes),(axis,axisMidPoint))
      
    else 
    printfn("%s") "LEAF"
    Leaf(shapes, (mkKdBbox shapes))
