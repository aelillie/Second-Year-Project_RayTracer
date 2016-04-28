namespace mkShape


open Ray
open Vector
open System.Drawing
open Point
open Texture
open Material
open Transformation

module EditShape = 
    let pi = System.Math.PI
    let pow (x, y) = System.Math.Pow(x, y)


    type shape2 = mkBasicShapes.shape2

    let transform s tr :shape2 = 
        let hit (R(p,d)) =  let p' = transPoint (getInv tr) p //transformed Ray origin
                            let d' = transVector (getInv tr) d //transformed direction
                            match s (R(p', d')) with
                            | None -> None
                            | Some(dist, dir, mat) -> let dir' = transVector (transpose (getInv tr)) dir
                                                      Some(dist, dir', mat) 
        hit 

    let group s1 s2 :shape2 = 
        let hit (R(p,d)as ray) = 
                                   let hit1, hit2 = s1 ray, s2 ray
                                   match (hit1, hit2) with
                                   | (None, None) -> None
                                   | (hit1, None) -> hit1
                                   | (None, hit2) -> hit2
                                   | (Some(dist1, _, _), Some(dist2, _, _)) -> if dist1 > dist2
                                                                               then hit2
                                                                               else hit1
        hit


    let union s1 s2 :shape2 =
        let hit (R(p,d) as ray) = 
                                   let hit1, hit2 = s1 ray, s2 ray
                                   match (hit1, hit2) with
                                   | (None, None) -> None
                                   | (hit1, None) -> hit1
                                   | (None, hit2) -> hit2
                                   | (Some(dist1, _, _), Some(dist2, _, _)) -> if dist1 > dist2 
                                                                               then hit2
                                                                               else hit1

        hit


    let intersection s1 s2  =
        failwith "Not implemented"



    let subtraction s1 s2 : shape2 =
        failwith "Not implemented"
