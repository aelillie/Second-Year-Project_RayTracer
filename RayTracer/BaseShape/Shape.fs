module Shape
open Point
open Vector
open Ray
open ExprParse
open Material
open Transformation


///Entry point for transforming a shape
//Should call transHit or contain the logic itself


//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
let pi = System.Math.PI
type Shape =
  | S of Point * float * Material
  | TShape of Shape * Transformation
  | P of Point * Vector * Material
  | D of Point * float * Material
  | B of Shape list
  | HC of Point * float * float * Material
  | T of Point * Point * Point * Material
  | SC of Shape * Shape * Shape
  | Rec of Point * float * float * Material
  override s.ToString() =
    match s with
      |S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"
      |P(point,normVector, mat) -> "("+point.ToString()+","+normVector.ToString()+"," + mat.ToString() + ")"
      |T(a,b,c,mat) -> "("+a.ToString()+","+ b.ToString()+","+c.ToString()+","+mat.ToString()+")"


let pow (x, y) = System.Math.Pow(x, y)
let transform (s : Shape) (t : Transformation) = TShape(s, t)


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
        let backT = mergeTransformations [translate 0.0 0.0 depth; frontT;]
        let bottomT = mergeTransformations [frontT; rotateX (pi/2.0)]
        let topT = mergeTransformations [translate 0.0 height 0.0 ; bottomT; ]
        let leftT = mergeTransformations [frontT; rotateY (-(pi/2.0))]
        let rightT = mergeTransformations [translate width 0.0 0.0; leftT;]

        let transformations = [frontT; backT; bottomT; topT; leftT; rightT]

        let frontR = mkRectangle (mkPoint 0.0 0.0 0.0) width height front
        let backR =  mkRectangle (mkPoint 0.0 0.0 0.0) width height back
        let bottomR = mkRectangle (mkPoint 0.0 0.0 0.0) width depth bottom
        let topR = mkRectangle (mkPoint 0.0 0.0 0.0) width depth top
        let leftR = mkRectangle (mkPoint 0.0 0.0 0.0) depth height left
        let rightR = mkRectangle (mkPoint 0.0 0.0 0.0) depth height right

        let rectangles = [frontR;backR;bottomR;topR;leftR;rightR] 

        let rects = List.map2 (fun s t -> transform s t) rectangles transformations

        B(rects)


        
        
        
        
         
//Sphere
let mkSphere orego radius material = S (orego, radius, material)
let getSphereRadius (S(_,radius,_)) = radius
let getSphereMaterial (S(_, _, mat)) = mat

//Planes
let mkPlane point normVector material = P (point, normVector, material)
let getPlanePoint (P(point,_,_)) = point
let getPlaneNormVector (P(_,normVector,_)) = normVector
let getPlaneMaterial (P(_, _, mat)) = mat



//Cylinders and Discs
let mkHollowCylinder (c : Point) (r : float) (h : float) (t : Material) : Shape = HC(c,r,h,t)
let mkDisc (c : Point) (r : float) (t : Material) : Shape = D(c,r,t)
let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Material) (top : Material) (bottom : Material) : Shape
     = 
     let cyl = mkHollowCylinder c r h t 
     let botDisc = mkDisc c r bottom
     let topDisc = mkDisc c r top

     let transTop = mergeTransformations [translate 0.0 (h/2.0) 0.0; rotateX (-(pi/2.0))]
     let transBot = mergeTransformations [translate 0.0 (-h/2.0) 0.0; rotateX (-(pi/2.0)) ]
     let topDisc' = transform topDisc transTop
     let botDisc' = transform botDisc transBot

     SC(cyl,topDisc',botDisc')



//Hit function for Rectangle. Rectangle is AXis alligned with XY. Can be moved by transforming.
let hitRec (R(p,t,d)) (Rec(c,w,h,m)) = 
    let dz = Vector.getZ d
    let pz = Point.getZ p
    let distance = (-1.0 * pz) / dz
    let p' = Point.move p (Vector.multScalar d distance)
    
    let px = Point.getX p'
    let py = Point.getY p'
    let ax = Point.getX c
    let ay = Point.getY c


    if ax <= px && px <= (ax + w)  && ay <= py && py <= (ay + h)
    then Some(distance, Vector.mkVector 0.0 0.0 1.0, m)
    else None

//Triangle
let mkTriangle a b c mat = T(a,b,c,mat)
let getTriangleA (T(a,_,_,_)) = a
let getTriangleB (T(_,b,_,_)) = b
let getTriangleC (T(_,_,c,_)) = c
let getTriangleMat (T(_,_,_,mat)) = mat

//Hit function for disc always handles as if XY alligned and centre point in (0,0,0)
let hitDisc (R(p,t,d)) (D(c,r,m)) = 
    let dz = Vector.getZ d
    let pz = Point.getZ p
    let distance = (-1.0 * pz) / dz
    let p' = Point.move p (Vector.multScalar d distance)
    let result = (pow (Point.getX p', 2.0)) + (pow (Point.getY p', 2.0))

    if result <= (pow (r,2.0)) 
    then 
     Some(distance, Vector.mkVector 0.0 0.0 1.0, m)
    else 
     None

//Calculates if cylinder hit. Cylinder is always centeret on 0,0,0 and is XZ alligned.
let hitCylinder (R(p,t,d)) (HC(center,r,h,m)) = 
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
     
     if (h / (-2.0)) <= pyt1 && pyt1 <= (h / 2.0) && tlittle >= 0.0
     then 
        let px = Point.getX p + tlittle * Vector.getX d
        let pz = Point.getZ p + tlittle * Vector.getZ d
        Some(tlittle, Vector.mkVector (px / r) 0.0 (pz / r), m)
     elif (h / (-2.0)) <= pyt2 && pyt2 <= (h / 2.0) && tbig >= 0.0
     then
        let px = Point.getX p + tbig * Vector.getX d
        let pz = Point.getZ p + tbig * Vector.getZ d
        Some(tbig, Vector.mkVector (px / r) 0.0 (pz / r), m)
     else None

///should be rendered
let rec hit ((R(p,t,d)) as ray) (s:Shape) =
    match s with
    |S(o,r,mat) ->  let makeNV a = Point.move p (a * d) |> Point.direction o
    
                    
                    let a = (pow((Vector.getX d),2.0) +
                             pow((Vector.getY d),2.0) +
                             pow((Vector.getZ d),2.0))

                    let b =  (2.0 * (Point.getX p - Point.getX o) * Vector.getX d) +
                             (2.0 * (Point.getY p - Point.getY o) * Vector.getY d) +
                             (2.0 * (Point.getZ p - Point.getZ o) * Vector.getZ d)

                    let c =  pow((Point.getX p - Point.getX o),2.0) +
                             pow((Point.getY p - Point.getY o),2.0) +
                             pow((Point.getZ p - Point.getZ o),2.0) -
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

    |P(pVector,n, mat) -> let denom = Vector.dotProduct d n
                          if(denom > 0.0) then
                              let v = Point.distance p pVector
                              let result = Vector.dotProduct v n
                              Some (result, n, mat)
                          else None
    | TShape(s, tr) -> let p' = transPoint (getInv tr) p //transformed Ray origin
                       let d' = transVector (getInv tr) d //transformed direction
                       match hit (R(p', t, d')) s with
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
  
         ///Returns the distance to the hit point, t, the normal of the hit point, and the material of the hit point
             Some(t, vectorN v u, mat)
          else None //gamma + beta is less than 0 or greater than 1
        else None // Can't divide with zero

    |SC(c,top,bot) -> let min = List.map(fun x -> hit ray x) [c;top;bot] |> List.choose id
                      match min with
                      |[] -> None
                      |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
    |B(rects) -> let min = List.map(fun x -> hit ray x) rects |> List.choose id
                 match min with
                 |[] -> None
                 |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
    |Rec(_) as rect -> hitRec ray rect
