module Shape
open Point
open Vector
open Ray
open ExprParse
open Material
open ExprParse
open ExprToPoly


//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
// Implicit Surfaces:
// 1. Parsing
// 2. Plug in the ray equation
// 3. simplify equation
// 4. Find number of roots using Sturms
// 5. Find the roots using Newton


type baseShape =
  | BS of Point * float 
  

type Shape =
  | S of Point * float * Material
  | P of Point * Vector * Material
  override s.ToString() =
    match s with
      |S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"
      |P(point,normVector, mat) -> "("+point.ToString()+","+normVector.ToString()+"," + mat.ToString() + ")"

let mkSphere orego radius material = S (orego, radius, material)
let getSphereRadius (S(_,radius,_)) = radius
let getSphereMaterial (S(_, _, mat)) = mat
let mkPlane point normVector material = P (point, normVector, material)
let getPlanePoint (P(point,_,_)) = point
let getPlaneNormVector (P(_,normVector,_)) = normVector
let getPlaneMaterial (P(_, _, mat)) = mat


let mkImplicit (s : string) : baseShape = 
    //parse polynomial string
    let polExpr = parseStr s

    //replace x,y,z with the ray equations corresponding values
    let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
    let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
    let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))
    let eR = FNum -1.0
    let polX = subst polExpr ("x", ex)
    let polY = subst polExpr ("y", ey)
    let polZ = subst polExpr ("z", ez)
    
    //simplify equation
    failwith "bla"


                                          


///Given a ray, computes the hit point for a sphere,
//and returns information on how the point
///should be rendered
let hit (R(p,t,d)) (s:Shape) =
    match s with
    |S(o,r,mat) ->  let makeNV a = Point.move p (a * d) |> Point.direction o
    
                    let pow (x, y) = System.Math.Pow(x, y)
                    let a = (pow((Vector.getX d),2.0) +
                             pow((Vector.getY d),2.0) +
                             pow((Vector.getZ d),2.0))

                    let b = (2.0 * Point.getX p * Vector.getX d +
                             2.0 * Point.getY p * Vector.getY d +
                             2.0 * Point.getZ p * Vector.getZ d)

                    let c =  pow((Point.getX p),2.0) +
                             pow((Point.getY p),2.0) +
                             pow((Point.getZ p),2.0) -
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
             