module Point

type Vector = Vector.Vector
type Point =
  | P of float * float * float
  override p.ToString() =
    match p with
      P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"

let mkPoint x y z = P(x, y, z)
let getX (P(x,_,_)) = x
let getY (P(_,y,_)) = y
let getZ (P(_,_,z)) = z
let getCoord (P(x,y,z)) = (x, y, z)
let move (P(x, y, z)) v = 
    P(x+Vector.getX v, y+Vector.getY v, z+Vector.getZ v)
let distance (P(px,py,pz)) (P(qx,qy,qz)) = 
    Vector.mkVector (qx-px) (qy-py) (qz-pz)
let direction p q = Vector.normalise(distance p q)
let round (P(px,py,pz)) (d:int) =
    let r (c:float) = System.Math.Round(c, d)
    P(r px, r py, r pz)

type Point with
  static member ( + ) ((P(x,y,z)), f) = (P(x+f,y+f,z+f))
  static member ( - ) ((P(x,y,z)), f) = (P(x-f,y-f,z-f))
 
  static member ( + ) (P(ux,uy,uz),P(vx,vy,vz)) = P(ux+vx, uy+vy, uz+vz)
  static member ( / ) (P(ux,uy,uz), f) = P(ux/f, uy/f, uz/f)            