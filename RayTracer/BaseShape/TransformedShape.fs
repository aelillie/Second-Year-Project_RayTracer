namespace Shapes


open Ray
open Material
open Vector
open Point
open Transformation
open BasicShape

module TransformedShape = 

    type Shape = Shapes.BasicShape.Shape

    type TransformedShape(s:Shape, tr) = 
        interface Shape with
            member this.isInside p = failwith "Not Implemented"
            member this.isSolid () = s.isSolid()
            member this.hit (R(p,d)) = 
                                        let p' = transPoint (getInv tr) p //transformed Ray origin
                                        let d' = transVector (getInv tr) d //transformed direction
                                        match s.hit (R(p', d')) with
                                        | None -> None
                                        | Some(dist, dir, mat) -> let dir' = transVector (transpose (getInv tr)) dir
                                                                  Some(dist, dir', mat) 

    type GroupShape(s1:Shape,s2:Shape) = 
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.isSolid () = s1.isSolid() && s2.isSolid()
            member this.hit (R(p,d) as ray) =  let hit1, hit2 = s1.hit ray, s2.hit ray
                                               match (hit1, hit2) with
                                               | (None, None) -> None
                                               | (hit1, None) -> hit1
                                               | (None, hit2) -> hit2
                                               | (Some(dist1, _, _), Some(dist2, _, _)) -> if dist1 > dist2
                                                                                           then hit2
                                                                                           else hit1

    type UnionShape(s1:Shape, s2:Shape) = 
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = failwith "Not implemented"


    type IntersectionShape(s1:Shape, s2:Shape) = 
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = failwith "Not implemented"


    type SubtractionShape(s1:Shape, s2:Shape) =
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = failwith "Not implemented"

    
    let transform s tr = new TransformedShape(s,tr)