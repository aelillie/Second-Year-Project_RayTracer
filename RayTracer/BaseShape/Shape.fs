module Shape
open Point
open Vector
open Ray
open ExprParse
open Material
open Transformation
open PlyParse



///Entry point for transforming a shape
//Should call transHit or contain the logic itself


//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
let pi = System.Math.PI
type Shape =
  | S of Point * float * Material
  | TShape of Shape * Transformation
  | PL of Material * Point * Vector
  | D of Point * float * Material
  | B of Shape list
  | HC of Point * float * float * Material
  | T of Point * Point * Point * Material
  | SC of Shape * Shape * Shape
  | Rec of Point * float * float * Material
  | UniS of Shape * Shape
  | IntS of Shape * Shape
  | SubS of Shape * Shape
  | GroS of Shape * Shape
  | TM of Shape list
  override s.ToString() =
    match s with
      |S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"
      |T(a,b,c,mat) -> "("+a.ToString()+","+ b.ToString()+","+c.ToString()+","+mat.ToString()+")"


let pow (x, y) = System.Math.Pow(x, y)
let transform (s : Shape) (t : Transformation) = TShape(s, t)

let rec isSolid = function
    | S(_,_,_)  -> true
    | B(_)      -> true
    | SC(_,_,_) -> true
    | TShape(s,_) -> isSolid s
    | _         -> false

exception NotSolidShapeException
//Collect a group of shapes as one union
let group s1 s2 = GroS(s1, s2)      
                     
//Union compose two shapes
let union s1 s2  = if isSolid s1 && isSolid s2 then UniS(s1, s2)      
                   else raise NotSolidShapeException
//Keep the difference between two shapes
let intersection s1 s2  = if isSolid s1 && isSolid s2 then IntS(s1, s2)
                          else raise NotSolidShapeException
//Subtract s2 from s1 (s2-s1)
let subtraction s1 s2  = if isSolid s1 && isSolid s2 then SubS(s1, s2)
                         else raise NotSolidShapeException



//Plane
let mkPlane (material : Material) =
      let point = mkPoint 0.0 0.0 0.0
      let normVector = mkVector 0.0 -1.0 0.0
      PL (material,point,normVector)

//Rectangle
let mkRectangle (corner : Point) (width : float) (height : float) (t : Material) : Shape
    = Rec(corner, width, height, t)

//Box
let mkBox (low : Point) (high : Point) (front : Material) (back : Material) (top : Material) (bottom : Material) (left : Material) (right : Material) : Shape
      = let width = System.Math.Abs(Point.getX high - Point.getX low)
        let height = System.Math.Abs(Point.getY high - Point.getY low)
        let depth = System.Math.Abs(Point.getZ high - Point.getZ low)
        let az = System.Math.Min(Point.getZ high, Point.getZ low)

        let frontT = translate (Point.getX low) (Point.getY low) az 
        let backT =   mergeTransformations [translate 0.0 0.0 depth; frontT;]
        let bottomT = mergeTransformations [frontT; rotateX (pi/2.0)]
        let topT =    mergeTransformations [translate 0.0 height 0.0 ; bottomT; ]
        let leftT =   mergeTransformations [frontT; rotateY (-(pi/2.0))]
        let rightT =  mergeTransformations [translate width 0.0 0.0; leftT;]

        let transformations = [frontT; backT; bottomT; topT; leftT; rightT]

        let p = mkPoint 0.0 0.0 0.0
        let frontR =  mkRectangle p width height front
        let backR =   mkRectangle p width height back
        let bottomR = mkRectangle p width depth bottom
        let topR =    mkRectangle p width depth top
        let leftR =   mkRectangle p depth height left
        let rightR =  mkRectangle p depth height right

        let rectangles = [frontR;backR;bottomR;topR;leftR;rightR] 

        let rects = List.map2 (fun s t -> transform s t) rectangles transformations

        B(rects)
let mkBoxCenter front back top bottom left right = 
        mkBox (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) front back top bottom left right

//Sphere
let mkSphere (p : Point) (r : float) (m : Material) : Shape = S(p,r,m)
let mkSphereCenter (r : float) (m : Material) : Shape = mkSphere (mkPoint 0.0 0.0 0.0) r m



//Cylinders and Discs
let mkHollowCylinder (c : Point) (r : float) (h : float) (t : Material) : Shape = HC(c,r,h,t)
let mkHollowCylinderCenter r h t = mkHollowCylinder (mkPoint 0.0 0.0 0.0) r h t
let mkDisc (c : Point) (r : float) (t : Material) : Shape = D(c,r,t)
let mkDiscCenter r t = mkDisc (mkPoint 0.0 0.0 0.0) r t
let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Material) (top : Material) (bottom : Material) : Shape
     = 
     let cyl = mkHollowCylinder c r h t 
     let botDisc = mkDisc c r bottom
     let topDisc = mkDisc c r top

     let transTop = mergeTransformations [translate 0.0 (h/2.0) 0.0; rotateX (-(pi/2.0))]
     let transBot = mergeTransformations [translate 0.0 (-h/2.0) 0.0; rotateX ((pi/2.0)) ]
     let topDisc' = transform topDisc transTop
     let botDisc' = transform botDisc transBot

     SC(cyl,topDisc',botDisc')
///Construct solid cylinder in 0.0 0.0 0.0
let mkSolidCylinderCenter r h t top bottom = mkSolidCylinder (mkPoint 0.0 0.0 0.0) r h t top bottom



//Hit function for Rectangle. Rectangle is AXis alligned with XY. Can be moved by transforming.
let hitRec (R(p,d)) (Rec(c,w,h,m)) = 
    let dz = Vector.getZ d
    let pz = Point.getZ p
    let distance = (-1.0 * pz) / dz
    let p' = Point.move p (Vector.multScalar d distance)
    
    let px = Point.getX p'
    let py = Point.getY p'
    let ax = Point.getX c
    let ay = Point.getY c


    if ax <= px && px <= (ax + w)  && ay <= py && py <= (ay + h) && distance > 0.0
    then Some(distance, Vector.mkVector 0.0 0.0 1.0, m)
    else None

//Triangle
let mkTriangle a b c mat = T(a,b,c,mat)
let getTriangleA (T(a,_,_,_)) = a
let getTriangleB (T(_,b,_,_)) = b
let getTriangleC (T(_,_,c,_)) = c
let getTriangleMat (T(_,_,_,mat)) = mat

//Hit function for disc always handles as if XY alligned and centre point in (0,0,0)
let hitDisc (R(p,d)) (D(c,r,m)) = 
    let dz = Vector.getZ d
    let pz = Point.getZ p
    let distance = (-1.0 * pz) / dz
    let p' = Point.move p (Vector.multScalar d distance)
    let result = (pow (Point.getX p', 2.0)) + (pow (Point.getY p', 2.0))

    if result <= (pow (r,2.0)) && distance > 0.0
    then 
     Some(distance, Vector.mkVector 0.0 0.0 1.0, m)
    else 
     None

//Calculates if cylinder hit. Cylinder is always centeret on 0,0,0 and is XZ alligned.
let hitCylinder (R(p,d)) (HC(center,r,h,m)) = 
    let a = pow (Vector.getX d, 2.0) + pow (Vector.getZ d, 2.0)
    let b = (2.0 * Point.getX p * Vector.getX d) + (2.0 * Point.getZ p * Vector.getZ d)
    let c = pow(Point.getX p, 2.0) + pow(Point.getZ p, 2.0) - pow(r, 2.0)
    let dis = pow(b, 2.0) - (4.0 * a * c)

    if dis < 0.0 
    then None
    else 
     let (t1, t2) = (-b + System.Math.Sqrt(dis)) / (2.0*a), (-b - System.Math.Sqrt(dis)) / (2.0*a)
     let (tbig, tlittle) = System.Math.Max(t1,t2), System.Math.Min(t1,t2)
     let pyt1 = Point.getY p + tlittle * Vector.getY d
     let pyt2 = Point.getY p + tbig * Vector.getY d
     

     if (h / (-2.0)) <= pyt1 && pyt1 <= (h / 2.0) && tlittle > 0.0
     then 
        let px = Point.getX p + tlittle * Vector.getX d
        let pz = Point.getZ p + tlittle * Vector.getZ d
        Some(tlittle, Vector.mkVector (px / r) 0.0 (pz / r), m)
     elif (h / (-2.0)) <= pyt2 && pyt2 <= (h / 2.0) && tbig > 0.0
     then
        let px = Point.getX p + tbig * Vector.getX d
        let pz = Point.getZ p + tbig * Vector.getZ d
        Some(tbig, Vector.mkVector (px / r) 0.0 (pz / r), m)
     else None

let collectFaces = function
 |Face(x) -> [x]
 |_ -> []

let collectVertices = function
 |Vertex(floatList) -> [floatList]
 |_ -> []

let mkPointFromIndex p i list =
    let x = List.item 0 (List.item i list)
    let y = List.item 1 (List.item i list)
    let z = List.item 2 (List.item i list)
    Point.mkPoint (x - Point.getX p) (y - Point.getY p) (z - Point.getZ p)
let rnd = System.Random()    

let mkTriangleMesh p (plyList:Ply list) =
    let vertexList = plyList |> List.collect collectVertices
    let faceList = plyList |> List.collect collectFaces
    
    let rec makeTriangles vList fList = 
        match fList with
         |[] -> []
         |l::fList' ->  
                        let p1 = mkPointFromIndex p (List.item 0 l) vList
                        let p2 = mkPointFromIndex p (List.item 1 l) vList
                        let p3 = mkPointFromIndex p (List.item 2 l) vList
                        (mkTriangle p1 p2 p3 (Material.mkMaterial(Colour.fromColor System.Drawing.Color.Gray) 0.0))::makeTriangles vList fList'
    
    let tri = makeTriangles vertexList faceList

    TM(tri)

///Given a ray, computes the hit point for a sphere,
//and returns information on how the point
///should be rendered
let rec hit ((R(p,d)) as ray) (s:Shape) =
    match s with
    |S(o,r,mat) ->  let makeNV a = Point.move p (a * d) |> Point.direction o
    
                    let a = (pow((Vector.getX d),2.0) +
                             pow((Vector.getY d),2.0) +
                             pow((Vector.getZ d),2.0))

                    let b =  (2.0 * Point.getX p * Vector.getX d) +
                                (2.0 * Point.getY p * Vector.getY d) +
                                (2.0 * Point.getZ p * Vector.getZ d)

                    let c =  pow(Point.getX p,2.0) +
                                pow(Point.getY p,2.0) +
                                pow(Point.getZ p,2.0) -
                             pow(r,2.0)

                    let disc = System.Math.Pow(b,2.0) - 4.0 * a * c

                    if(disc < 0.0) then None
                    else
                        let answer1 = (-b + System.Math.Sqrt(disc)) / (2.0*a)
                        let answer2 = (-b - System.Math.Sqrt(disc)) / (2.0*a)
                        if answer1 < 0.0 && answer2 < 0.0 then None
                        else
            
                            let answer = System.Math.Min(answer1,answer2)
                            if answer < 0.0 
                            then 
                             let answer = System.Math.Max(answer1,answer2)
                             Some (answer, makeNV answer, mat)
                            else Some (answer, makeNV answer, mat)

    |PL(mat,pVector,n) -> let denom = Vector.dotProduct (Vector.normalise d) (Vector.normalise n)
                          if(denom > 0.0000001) then
                              let v = Point.distance p pVector
                              let result = (Vector.dotProduct v n) / denom 
                              if result >= 0.0 then Some (result, n, mat)
                              else None
                          else None
    | TShape(s, tr) -> let p' = transPoint (getInv tr) p //transformed Ray origin
                       let d' = transVector (getInv tr) d //transformed direction
                       match hit (R(p', d')) s with
                       | None -> None
                       | Some(dist, dir, mat) -> let dir' = transVector (transpose (getInv tr)) dir
                                                 Some(dist, dir', mat)
    |D(_) as disc -> hitDisc ray disc

    |HC(_) as hc -> hitCylinder ray hc
    | T(a,b,c,mat) -> 

        let u = Vector.mkVector ((Point.getX b) - (Point.getX a)) ((Point.getY b) - (Point.getY a)) ((Point.getZ b) - (Point.getZ a))
        let v = Vector.mkVector ((Point.getX c) - (Point.getX a)) ((Point.getY c) - (Point.getY a)) ((Point.getZ c) - (Point.getZ a))

        //Function to find the normal of the triangle
        let vectorN a b = Vector.normalise (Vector.crossProduct a b)

        let a1 = (Point.getX a) - (Point.getX b)
        let b1 = (Point.getX a) - (Point.getX c)
        let c1 = Vector.getX d
        let d1 = (Point.getX a) - (Point.getX p)

        let e = (Point.getY a) - (Point.getY b)
        let f = (Point.getY a) - (Point.getY c)
        let g = Vector.getY d
        let h = (Point.getY a) - (Point.getY p)
    
        let i = (Point.getZ a) - (Point.getZ b)
        let j = (Point.getZ a) - (Point.getZ c)
        let k = Vector.getZ d
        let l = (Point.getZ a) - (Point.getZ p)

        let D = a1*(f*k - g*j) + b1*(g*i-e*k) + c1*(e*j-f*i) 

        //Find the unknowns
        //If D!=0 we have a solution    
        if (D <> 0.0)  then 
          let beta = (d1*(f*k-g*j)+b1*(g*l-h*k)+c1*(h*j-f*l))/D  //x
          let gamma = (a1*(h*k-g*l)+d1*(g*i-e*k)+c1*(e*l-h*i))/D //y
          let t = (a1*(f*l-h*j)+b1*(h*i-e*l)+d1*(e*j-f*i))/D     //z
             
          if beta >= 0.0 && gamma >= 0.0 && gamma+beta <= 1.0
           then 
             let p' = Point.move a ((Vector.multScalar u beta) + (Vector.multScalar v gamma))
  
             //Returns the distance to the hit point, t, the normal of the hit point, and the material of the hit point
             if t > 0.0 
             then Some(t, vectorN v u, mat)
             else None
          else None //gamma + beta is less than 0 or greater than 1
        else None // Can't divide with zero

    |SC(c,top,bot) -> let hits = List.map(fun x -> hit ray x) [c;top;bot]
                      let min = hits |> List.choose id
                      match min with
                      |[] -> None
                      |_ ->  Some(List.minBy (fun (di, nV, mat) -> di) min) 

    |B(rects) -> let min = List.map(fun x -> hit ray x) rects |> List.choose id
                 match min with
                 |[] -> None
                 |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
    |TM(rects) -> let min = List.map(fun x -> hit ray x) rects |> List.choose id
                  match min with
                  |[] -> None
                  |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
    |Rec(_) as rect -> hitRec ray rect
    | UniS(s1, s2)  -> let hit1, hit2 = hit ray s1, hit ray s2
                       match (hit1, hit2) with
                       | (None, None) -> None
                       | (hit1, None) -> hit1
                       | (None, hit2) -> hit2
                       | (Some(dist1, _, _), Some(dist2, _, _)) -> if dist1 > dist2 
                                                                   then hit2
                                                                   else hit1
    | GroS(s1, s2)  -> let hit1, hit2 = hit ray s1, hit ray s2
                       match (hit1, hit2) with
                       | (None, None) -> None
                       | (hit1, None) -> hit1
                       | (None, hit2) -> hit2
                       | (Some(dist1, _, _), Some(dist2, _, _)) -> if dist1 > dist2
                                                                   then hit2
                                                                   else hit1