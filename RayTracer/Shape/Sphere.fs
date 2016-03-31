module Sphere

open Point
open Vector
open Ray
open TracerAPI
open ExprParse
(*
    A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
 *)

type Sphere =
  | R of Point * float * texture
  override r.ToString() =
    match r with
      R(radius) -> "("+r.ToString()+")"

let mkSphere radius = R (radius)
let getRadius (R(radius)) = radius

let hit (ray:Ray) (sphere:Sphere) =
    let p = Ray.getP ray
    let t = Ray.getT ray
    let d = Ray.getD ray

    let a = (System.Math.Pow((Vector.getX d),2.0) +
             System.Math.Pow((Vector.getY d),2.0) +
             System.Math.Pow((Vector.getZ d),2.0)) * System.Math.Pow(t,2.0)

    let b = (2.0 * Point.getX p * Vector.getX d +
             2.0 * Point.getY p * Vector.getY d +
             2.0 * Point.getZ p * Vector.getZ d) * t

    let c =  System.Math.Pow((Point.getX p),2.0) +
             System.Math.Pow((Point.getY p),2.0) +
             System.Math.Pow((Point.getZ p),2.0) -
             System.Math.Pow(Sphere. ,2.0)

    let d = System.Math.Pow(b,2.0) + 4.0 * a * c

    if(d < 0.0) then (false,0.0,Vector.mkVector(0.0),0.0)
    else
    let answer1 = (-b + System.Math.Sqrt(d)) / (2.0*a)
    let answer2 = (-b - System.Math.Sqrt(d)) / (2.0*a)
    if  answer1 >= answer2 then (true,answer2,Vector.mkVector(0.0),0.0)
                           else (true,answer1,Vector.mkVector(0.0),0.0)



    

(*
The distance to the hit point
• The normal of the hit point (A vector
perpendicular to the surface at the hit
point)
• The material at the hit point (for our
purposes colour and reflective
property)
*)
 