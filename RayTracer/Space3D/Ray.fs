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
