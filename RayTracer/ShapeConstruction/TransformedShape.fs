namespace Shapes

open Ray
open Material
open Vector
open Point
open Transformation
open BasicShape

module TransformedShape = 

    let epsilon = 0.0000001

    type TransformedShape(s:Shape, tr) = 
        interface Shape with
            member this.isInside p = s.isInside (transPoint (getInv tr) p)
            member this.getBounding () = match s.getBounding () with
                                         | None -> None //E.g. a plane
                                         | Some b ->
                                            let (p1, p2) = b.getL, b.getH
                                            let (x1, y1, z1), (x2, y2, z2) = getCoord p1, getCoord p2
                                            //Bottom vertices:
                                            let b = p1 //left low (2D rectangle)
                                            let by = mkPoint x1 y2 z1 //right low
                                            let bzy = mkPoint x1 y2 z2 //left top
                                            let bz = mkPoint x1 y1 z2 //right top
                                            //Top vertices
                                            let ty = mkPoint x2 y1 z2 //left low
                                            let tzy = mkPoint x2 y1 z1 //right low
                                            let tz = mkPoint x2 y2 z1 //left top
                                            let t = p2 //right top
                                            let vertices = [b;by;bzy;bz;ty;tzy;tz;t]
                                            let vertices' = List.map //Transform vertices
                                                             (fun v -> transPoint (getT tr) v) vertices
                                            //Bottom lowest coordinates
                                            let minX = List.minBy (fun p -> getX p) vertices' |> getX
                                            let minY = List.minBy (fun p -> getY p) vertices' |> getY
                                            let minZ = List.minBy (fun p -> getZ p) vertices' |> getZ
                                            //Top highest coordinates
                                            let maxX = List.maxBy (fun p -> getX p) vertices' |> getX
                                            let maxY = List.maxBy (fun p -> getY p) vertices' |> getY
                                            let maxZ = List.maxBy (fun p -> getZ p) vertices' |> getZ
                                            let p1' = mkPoint minX minY minZ
                                            let p2' = mkPoint maxX maxY maxZ
                                            Some {p1 = p1'; p2 = p2'}
            member this.isSolid () = s.isSolid()
            member this.hit (R(p,d)) = 
                                        let p' = transPoint (getInv tr) p //transformed Ray origin
                                        let d' = transVector (getInv tr) d //transformed direction
                                        match s.hit (R(p', d')) with
                                        | None -> None
                                        | Some(dist, dir, mat) -> let dir' = (transVector (transpose (getInv tr)) dir) |> normalise
                                                                  Some(dist, dir', mat) 
    let transform s tr = new TransformedShape(s,tr):> Shape

    //Computes the bounding box of a combined shape
    let makeBounding (s1:Shape) (s2:Shape) = 
                             let min (x:float) (y:float) = System.Math.Min (x,y)
                             let max (x:float) y = System.Math.Max (x,y)
                             let b1, b2 = (s1.getBounding ()).Value, (s2.getBounding ()).Value
                             let (lx1,ly1,lz1) = (Point.getCoord b1.p1) //Low point of s1
                             let (lx2,ly2,lz2) = (Point.getCoord b2.p1) //Low point of s2
                             let (hx1,hy1,hz1) = (Point.getCoord b1.p2) //High point of s2
                             let (hx2,hy2,hz2) = (Point.getCoord b2.p2) //High point of s2
                             let (lx,ly,lz) =  min lx1 lx2, min ly1 ly2, min lz1 lz2     
                             let hx, hy, hz = max hx1 hx2, max hy1 hy2, max hz1 hz2
                             {p1 = (mkPoint lx ly lz); p2 = (mkPoint hx hy hz)}

    type GroupShape(s1:Shape,s2:Shape) = 
        interface Shape with
            member this.isInside p = s1.isInside p || s2.isInside p
            member this.getBounding () = Some(makeBounding s1 s2)
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
        let hit (R(p,d) as ray) (s:Shape) = s.hit ray
        interface Shape with
            member this.isInside p = s1.isInside p || s2.isInside p
            member this.getBounding () = Some (makeBounding s1 s2)
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
                             then if dist1 < dist2+epsilon then hit1 else hit2 
                             else let dist = if dist1 < dist2+epsilon then dist1 else dist2
                                  let newPoint = move p ((dist+epsilon) * d) //Inside a shape
                                  let newRay = mkRay newPoint d //Origin on the other side of surface
                                  match hit newRay this with
                                  | Some(d,v,m) -> Some(dist+epsilon+d,v,m)
                                  | _ -> None


    type IntersectionShape(s1:Shape, s2:Shape) = 
        let hit (R(p,d) as ray) (s:Shape) = s.hit ray
        interface Shape with
            member this.isInside p = s1.isInside p && s2.isInside p
            member this.getBounding () = 
                let b1, b2 = (s1.getBounding ()).Value, (s2.getBounding ()).Value
                let (lx1,ly1,lz1) = (Point.getCoord b1.p1) //Low point of s1
                let (lx2,ly2,lz2) = (Point.getCoord b2.p1) //Low point of s2
                let (hx1,hy1,hz1) = (Point.getCoord b1.p2) //High point of s2
                let (hx2,hy2,hz2) = (Point.getCoord b2.p2) //High point of s2
                let lowPoint = if lx1 < lx2 && ly1 < ly2 && lz1 < lz2
                               then b2.p1 else b1.p1 //Choose the highest low point
                let highPoint = if hx1 < hx2 && hy1 < hy2 && hz1 < hz2
                                then b1.p2 else b2.p2 //Choose the lowest high point
                Some{p1 = lowPoint; p2 = highPoint}
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                      let hit1, hit2 = s1.hit ray, s2.hit ray
                      match (hit1, hit2) with
                      | Some(dist1, _, _), 
                        Some(dist2, _, _) -> 
                            match ((s1.isInside p), (s2.isInside p)) with
                            | (true, true) -> if dist1 < (dist2+epsilon) then hit1 else hit2
                            | (true, false) -> if (s1.isInside (move p ((dist2+epsilon) * d)))
                                               then hit2 else None 
                            | (false, true) -> if (s2.isInside (move p ((dist1+epsilon) * d)))
                                               then hit1 else None
                            | (false, false) -> let dist = if dist1 < (dist2+epsilon) then dist1 else dist2
                                                let newPoint = move p ((dist+epsilon) * d)
                                                let newRay = mkRay newPoint d
                                                match hit newRay this with
                                                | Some(d,v,m) -> Some(dist+epsilon+d,v,m)
                                                | _ -> None
                      | _ -> None


    type SubtractionShape(s1:Shape, s2:Shape) =
        let hit (R(p,d) as ray) (s:Shape) = s.hit ray
        interface Shape with
            member this.isInside p = s1.isInside p && (not (s2.isInside p))
            member this.getBounding () = s1.getBounding ()
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                      let hit1, hit2 = s1.hit ray, s2.hit ray
                      match (hit1, hit2) with
                      | (hit1, None) -> hit1 //Only hit s1
                      | (Some(dist1, _, _), //Hit both shapes
                         Some(dist2, _, _)) -> 
                            let hp1, hp2 = move p ((dist1+epsilon) * d), move p ((dist2+epsilon) * d)
                            if (s1.isInside hp2) && dist2 < (dist1+epsilon)
                            then hit2
                            else if (not (s2.isInside hp1)) then hit1
                                 else
                                 let dist = if dist1 < (dist2+epsilon) then dist1 else dist2
                                 let newPoint = move p ((dist+epsilon) * d)
                                 let newRay = mkRay newPoint d
                                 match hit newRay this with
                                 | Some(d,v,m) -> Some(dist+epsilon+d,v,m)
                                 | _ -> None
                      | _ -> None //No hit, or only s2
