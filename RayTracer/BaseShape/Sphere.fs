﻿module Shape
open Point
open Vector
open Ray
open ExprParse
open Material
(*
    A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
 *)

type Sphere =
  | S of Point * float * Material
  override s.ToString() =
    match s with
      S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"

let mkSphere orego radius material = S (orego, radius, material)
let getRadius (S(_,radius,_)) = radius
let getMaterial (S(_, _, mat)) = mat

let findNormalV t (d:Vector) (p:Point) (o:Point) : Vector = 
    let p' = Vector.multScalar d t |> Point.move p 


    Point.direction o p'



let hit (R(x,y,p,t,d)) (S(o,r, mat)) =

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

    let dis = System.Math.Pow(b,2.0) - 4.0 * a * c

    if(dis < 0.0) then None
    else
    let answer1 = (-b + System.Math.Sqrt(dis)) / (2.0*a)
    let answer2 = (-b - System.Math.Sqrt(dis)) / (2.0*a)
    if  answer1 >= answer2 then let nV = findNormalV answer2 d p o
                                Some(x,y, answer2, nV, Material.getColour mat)
                           else let nV = findNormalV answer1 d p o
                                Some(x,y, answer2, nV, Material.getColour mat) //remember to return answer 






(*
The distance to the hit point
• The normal of the hit point (A vector
perpendicular to the surface at the hit
point)
• The material at the hit point (for our
purposes colour and reflective
property)
*)
 