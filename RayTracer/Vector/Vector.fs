module Vector

type Vector =
  | V of float * float * float
  override v.ToString() =
    match v with
      V(x,y,z) -> "["+x.ToString()+","+y.ToString()+","+z.ToString()+"]"

let mkVector x y z = V (x, y, z)
let getX (V(x,_,_)) = x
let getY (V(_,y,_)) = y
let getZ (V(_,_,z)) = z
let getCoord (V(x, y, z)) = (x, y, z)
let multScalar (V(x, y, z)) s = let f n = n * s in V(f x, f y, f z)
let magnitude (V(x, y, z)) = sqrt(x**2.0 + y**2.0 + z**2.0)
let dotProduct (V(x1, y1, z1)) (V(x2, y2, z2)) = (x1*x2)+(y1*y2)+(z1*z2)
let crossProduct (V(x1, y1, z1)) (V(x2, y2, z2)) = 
    V(x1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2)
let normalise (V(x,y,z) as v) = let a = magnitude v in V(x/a, y/a, z/a)
let round (V(x,y,z)) (d:int) = 
    V(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d))

type Vector with
  static member ( ~- ) (V(x,y,z)) = V(-x,-y,-z)
  static member ( + ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux+vx, uy+vy, uz+vz)
  static member ( - ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux-vx, uy-vy, uz-vz)
  static member ( * ) (s, v) = multScalar v s
  static member ( * ) (u, v) = dotProduct u v
