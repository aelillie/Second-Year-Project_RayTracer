module Ray
open Point
open Vector
(*
    A Ray has the function p + td
    p = a point
    t = scalar distance (typically a double)
    d = normalized direction vector
 *)

 type Ray(p:Point, t:double, d:Vector) = 
    member this.P = p
    member this.T = t
    member this.D = d