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
    | R of int * int * Point * double * Vector
    override R.ToString() =
        match R with
        R(x,y,p,t,d) -> "("+x.ToString()+","+y.ToString()+","+p.ToString()+","+t.ToString()+","+ d.ToString() + ")"
let mkRay x y p t d = R(x,y,p,t,d)

let getP (R(_, _, p,_,_)) = p
let getT (R(_, _, _,t,_)) = t
let getD (R(_, _, _,_,d)) = d
