module Ray
open Point
open Vector
(*
    A Ray has the function p + td
    p = a point
    t = scalar distance (typically a double)
    d = normalized direction vector
 *)
type Ray =
    | R of Point *  Vector
    override R.ToString() =
        match R with
        R(p,d) -> "("+p.ToString()+","+ d.ToString() + ")"
let mkRay p d = R(p,d)

let getP (R(p,_)) = p
let getD (R(_,d)) = d
let getValues (R(p,d)) = (p,d)
let getDirection (R(p,d)) axis =
    match axis with
    |"x" -> Vector.getX d
    |"y" -> Vector.getY d
    |"z" -> Vector.getZ d
let getOrigin (R(p,d)) axis =
    match axis with
    |"x" -> Point.getX p
    |"y" -> Point.getY p
    |"z" -> Point.getZ p
