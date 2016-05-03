namespace Shapes


open Ray
open Material
open Vector
open Point
open Transformation
open BasicShape

module TransformedShape = 

    let epsilon = 0.00001

    type TransformedShape(s:Shape, tr) = 
        interface Shape with
            member this.isInside p = s.isInside (transPoint (getInv tr) p)
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = s.isSolid()
            member this.hit (R(p,d)) = 
                                        let p' = transPoint (getInv tr) p //transformed Ray origin
                                        let d' = transVector (getInv tr) d //transformed direction
                                        match s.hit (R(p', d')) with
                                        | None -> None
                                        | Some(dist, dir, mat) -> let dir' = transVector (transpose (getInv tr)) dir
                                                                  Some(dist, dir', mat) 
    let transform s tr = new TransformedShape(s,tr)

    type GroupShape(s1:Shape,s2:Shape) = 
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = failwith "Not implemented"
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
        member this.hit (R(p,d) as ray) = this.hit ray
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                       let hit1, hit2 = s1.hit ray, s2.hit ray
                       match (hit1, hit2) with
                       | (None, None) -> None
                       | (hit1, None) -> hit1
                       | (None, hit2) -> hit2
                       | Some(dist1, v1, _), 
                         Some(dist2, v2, _) -> 
                             if (not (s1.isInside p)) && (not (s2.isInside p))
                             then if dist1 < dist2 then hit1 else hit2 
                             else let dist = if dist1 < dist2 then dist1 else dist2
                                  let newPoint = move p ((dist+epsilon) * d) //Inside a shape
                                  let newRay = mkRay newPoint d //Origin on the other side of surface
                                  match this.hit newRay with
                                  | Some(d,v,m) -> Some(dist1+epsilon+d,v,m)
                                  | _ -> None


    type IntersectionShape(s1:Shape, s2:Shape) = 
        member this.hit (R(p,d) as ray) = this.hit ray
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                      let hit1, hit2 = s1.hit ray, s2.hit ray
                      match (hit1, hit2) with
                      | Some(dist1, _, _), 
                        Some(dist2, _, _) -> 
                            match ((s1.isInside p), (s2.isInside p)) with
                            | (true, true) -> if dist1 < dist2 then hit1 else hit2
                            | (true, false) -> if (s1.isInside (move p (dist2 * d)))
                                               then hit2 else None 
                            | (false, true) -> if (s2.isInside (move p (dist1 * d)))
                                               then hit1 else None
                            | (false, false) -> let dist = if dist1 < dist2 then dist1 else dist2
                                                let newPoint = move p ((dist+epsilon) * d)
                                                let newRay = mkRay newPoint d
                                                match this.hit newRay with
                                                | Some(d,v,m) -> Some(dist1+epsilon+d,v,m)
                                                | _ -> None
                      | _ -> None


    type SubtractionShape(s1:Shape, s2:Shape) =
        member this.hit (R(p,d) as ray) = this.hit ray
        interface Shape with
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                      let hit1, hit2 = s1.hit ray, s2.hit ray
                      match (hit1, hit2) with
                      | (hit1, None) -> hit1 //Only hit s1
                      | (Some(dist1, _, _), //Hit both shapes
                         Some(dist2, _, _)) -> 
                            let hp1, hp2 = move p (dist1 * d), move p (dist2 * d)
                            if (s1.isInside hp2) then if dist1 < dist2 then hit1 else hit2
                            else if (not (s2.isInside hp1)) then hit1
                                 else
                                 let newPoint = move p ((dist1+epsilon) * d)
                                 let newRay = mkRay newPoint d
                                 match this.hit newRay with
                                 | Some(d,v,m) -> Some(dist1+epsilon+d,v,m)
                                 | _ -> None
                      | _ -> None //No hit, or only s2
