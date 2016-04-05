module Shape

open Point
open Vector
open Ray
open ExprParse
(*
    A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
 *)

type Sphere =
  | S of Point * float
  override s.ToString() =
    match s with
      S(orego,radius) -> "("+orego.ToString()+","+radius.ToString()+")"

let mkSphere orego radius = S (orego,radius)
let getRadius (S(_,radius)) = radius

let hit (R(x,y,p,t,d)) (S(_,r)) =

    let a = (System.Math.Pow((Vector.getX d),2.0) +
             System.Math.Pow((Vector.getY d),2.0) +
             System.Math.Pow((Vector.getZ d),2.0))

    let b = (2.0 * Point.getX p * Vector.getX d +
             2.0 * Point.getY p * Vector.getY d +
             2.0 * Point.getZ p * Vector.getZ d)

    let c =  System.Math.Pow((Point.getX p),2.0) +
             System.Math.Pow((Point.getY p),2.0) +
             System.Math.Pow((Point.getZ p),2.0) -
             System.Math.Pow(r,2.0)

    let d = System.Math.Pow(b,2.0) - 4.0 * a * c

    if(d < 0.0) then (false,x,y,System.Drawing.Color.Black)
    else
    let answer1 = (-b + System.Math.Sqrt(d)) / (2.0*a)
    let answer2 = (-b - System.Math.Sqrt(d)) / (2.0*a)
    if  answer1 >= answer2 then (true,x,y,System.Drawing.Color.Blue)
                           else (true,x,y,System.Drawing.Color.Blue) //remember to return answer 



    

(*
The distance to the hit point
• The normal of the hit point (A vector
perpendicular to the surface at the hit
point)
• The material at the hit point (for our
purposes colour and reflective
property)
*)
 