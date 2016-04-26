module Plane

open Point
open Vector
open Ray
open Material

type Plane =
  | P of Point * Vector * Material
  override p.ToString() =
    match p with
      P(point,normVector, mat) -> "("+point.ToString()+","+normVector.ToString()+"," + mat.ToString() + ")"

let mkPlane point normVector material = P (point, normVector, material)
let getPoint (P(point,_,_)) = point
let getNormVector (P(_,normVector,_)) = normVector
let getMaterial (P(_, _, mat)) = mat

let planeHit (R(p,d)) (P(pVector,n, mat)) =
    let denom = Vector.dotProduct d n

    if(denom > 0.0) then
        let v = Point.distance p pVector
        let result = Vector.dotProduct v n
        result >= 0.0
    else false