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
    | R of Point * double * Vector
    override R.ToString() =
        match R with
        R(p,t,d) -> "("+p.ToString()+","+t.ToString()+","+ d.ToString() + ")"
let mkRay p t d = R(p,t,d)

let getP (R(p,_,_)) = p
let getT (R(_,t,_)) = t
let getD (R(_,_,d)) = d
