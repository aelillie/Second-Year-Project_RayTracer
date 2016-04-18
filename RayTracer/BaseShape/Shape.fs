﻿module Shape
open Point
open Vector
open Ray
open ExprParse
open Material
open Transformation



//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
let pi = System.Math.PI
type Shape =
  | S of Point * float * Material
  | TShape of Shape * Transformation
  | P of Point * Vector * Material
  | D of Point * float * Material
  | B 
  | HC of Point * float * float * Material
  | SC of Shape * Shape * Shape
  | Rec of Point * float * float * Material
  override s.ToString() =
    match s with
      |S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"
      |P(point,normVector, mat) -> "("+point.ToString()+","+normVector.ToString()+"," + mat.ToString() + ")"

let pow (x, y) = System.Math.Pow(x, y)
//Rectangle
let mkRectangle (corner : Point) (width : float) (height : float) (t : Material) : Shape
    = Rec(corner, width, height, t)
//Box
let mkBox (low : Point) (high : Point) (front : Material) (back : Material) (top : Material) (bottom : Material) (left : Material) (right : Material) : Shape
      = failwith "mkBox not implemented"
//Sphere
let mkSphere orego radius material = S (orego, radius, material)
let getSphereRadius (S(_,radius,_)) = radius
let getSphereMaterial (S(_, _, mat)) = mat

//Planes
let mkPlane point normVector material = P (point, normVector, material)
let getPlanePoint (P(point,_,_)) = point
let getPlaneNormVector (P(_,normVector,_)) = normVector
let getPlaneMaterial (P(_, _, mat)) = mat


///Entry point for transforming a shape
//Should call transHit or contain the logic itself
let transform (s : Shape) (t : Transformation) = TShape(s, t)

//Cylinders and Discs
let mkHollowCylinder (c : Point) (r : float) (h : float) (t : Material) : Shape = HC(c,r,h,t)
let mkDisc (c : Point) (r : float) (t : Material) : Shape = D(c,r,t)
let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Material) (top : Material) (bottom : Material) : Shape
     = 
     let cyl = mkHollowCylinder c r h t 
     let botDisc = mkDisc c r bottom
     let topDisc = mkDisc c r top

     let transTop = mergeTransformations [rotateX (-(pi/2.0));translate 0.0 (h/2.0) 0.0;]
     let transBot = mergeTransformations [rotateX (-(pi/2.0));translate 0.0 (-h/2.0) 0.0; ]
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
        let px = Point.getX p * tlittle * Vector.getX d
        let pz = Point.getZ p * tlittle * Vector.getZ d
        Some(tlittle, Vector.mkVector (px / r) 0.0 (pz / r), m)
     elif (h / (-2.0)) <= pyt2 && pyt2 <= (h / 2.0) && tbig >= 0.0
     then
        let px = Point.getX p * tbig * Vector.getX d
        let pz = Point.getZ p * tbig * Vector.getZ d
        Some(tbig, Vector.mkVector (px / r) 0.0 (pz / r), m)
     else None

 
///Given a ray, computes the hit point for a shape,
///and returns information on how the point
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
             
    |Rec(_) as rect -> hitRec ray rect

    |SC(c,top,bot) -> let min = List.map(fun x -> hit ray x) [c;top;bot] |> List.choose id
                      match min with
                      |[] -> None
                      |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
