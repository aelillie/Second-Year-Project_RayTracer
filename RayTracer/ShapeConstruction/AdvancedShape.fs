namespace Shapes

open Ray
open Material
open Vector
open Point
open Transformation
open BasicShape
open TransformedShape
open PlyParse
open TmKdtree

module AdvancedShape = 
    

    let bBoxFromList xs = 
        let sbbox = List.map (fun (c:Shape) -> c.getBounding().Value) xs

        let bL = List.map (fun (b:BasicShape.BoundingBox) -> b.getL) sbbox
        let bH = List.map (fun (b:BasicShape.BoundingBox) -> b.getH) sbbox

        let minX = List.minBy (fun x -> Point.getX x) bL
        let minY = List.minBy (fun x -> Point.getY x) bL
        let minZ = List.minBy (fun x -> Point.getZ x) bL

        let maxX = List.maxBy (fun x -> Point.getX x) bH
        let maxY = List.maxBy (fun x -> Point.getY x) bH
        let maxZ = List.maxBy (fun x -> Point.getZ x) bH
        Some {p1=(mkPoint (Point.getX minX - (epsilon*2.0)) (Point.getY minY - (epsilon*2.0)) (Point.getZ minZ - (epsilon *2.0)) ) 
        ; p2=(mkPoint (Point.getX maxX + (epsilon * 2.0)) (Point.getY maxY + (epsilon*2.0)) (Point.getZ maxZ + (epsilon * 2.0)) )}

        

    type Box(low,high,front,back,top,bottom,left,right) = 
        let rects = 
                let width = System.Math.Abs(Point.getX high - Point.getX low)
                let height = System.Math.Abs(Point.getY high - Point.getY low)
                let depth = System.Math.Abs(Point.getZ high - Point.getZ low)
                let az = System.Math.Max(Point.getZ high, Point.getZ low)

                let frontT = translate (Point.getX low) (Point.getY low) az 
                let backT =   mergeTransformations [frontT;translate 0.0 0.0 -depth]
                let bottomT = mergeTransformations [rotateX (pi/(-2.0));frontT]
                let topT =    mergeTransformations [bottomT;translate 0.0 height 0.0]
                let leftT =   mergeTransformations [rotateY ((pi/2.0));frontT]
                let rightT =  mergeTransformations [leftT;translate width 0.0 0.0]

                let transformations = [frontT; backT; bottomT; topT; leftT; rightT]
                
                let p = mkPoint 0.0 0.0 0.0
                let frontR =  new Rectangle (p, width, height, front)
                let backR =   new Rectangle (p, width, height, back)
                let bottomR = new Rectangle (p, width, depth, bottom)
                let topR =    new Rectangle (p, width, depth, top)
                let leftR =   new Rectangle (p, depth, height, left)
                let rightR =  new Rectangle (p, depth, height, right)

                [frontR;backR;bottomR;topR;leftR;rightR] 
                |> List.map2 (fun t s -> new TransformedShape (s, t) :> Shape) transformations

        member this.hit (R(p,d) as ray) = 
                    let min = List.map(fun (x:Shape) -> x.hit ray) rects |> List.choose id
                    match min with
                        |[] -> None
                        |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
        interface Shape with
            member this.isInside p = 
                                     let (x,y,z) = Point.getCoord p
                                     let (lx,ly,lz) = Point.getCoord low
                                     let (hx,hy,hz) = Point.getCoord high
                                     lx < x && x < hx && ly < y && y < hy && lz < z && z < hz 
            member this.getBounding () = bBoxFromList rects
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = this.hit ray 



    type SolidCylinder(c,r,h,t,top,bottom) = 
        let cyl = 
            let cyl' = (new HollowCylinder (c, r, h, t)) :> Shape
            let botDisc = new Disc (c, r, bottom)
            let topDisc = new Disc (c, r, top)

            let transTop = mergeTransformations [rotateX (-(pi/2.0));translate 0.0 (h/2.0) 0.0]
            let transBot = mergeTransformations [rotateX ((pi/2.0));translate 0.0 (-h/2.0) 0.0]
            let top' = (transform topDisc transTop)
            let bot' = (transform botDisc transBot)
            [cyl';bot';top']

        interface Shape with
            member this.isInside p = let (x,y,z) = Point.getCoord p
                                     let (cx, cy, cz) = Point.getCoord c
                                     let lowH, highH = cy-(h/2.0), cy+(h/2.0)
                                     ((x-cx)**2.0 + (z-cz)**2.0) < r**2.0
                                     && lowH < y && y < highH
            member this.getBounding () = let cylinder = cyl.[0]
                                         cylinder.getBounding()
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                            let hits = List.map(fun (x:Shape) -> x.hit ray) cyl
                            let min = hits |> List.choose id
                            match min with
                            |[] -> None
                            |_ ->  Some(List.minBy (fun (di, nV, mat) -> di) min) 



    type TriangleMesh (plyList, texture, smooth) = 
        let vertices = vertices plyList
        let faces = faces plyList
        let getCoord vi i = let vertex = vertices.[vi] in vertex.[i]
        let uvCoords v = match textureIndexes plyList with
                         | None -> None
                         | Some(ui, vi) -> Some(getCoord v ui, getCoord v vi) //Somehow swapped around
        let vertex vi = match XYZIndexes plyList with
                        | None -> failwith "No x y z coordinates in PLY"
                        | Some(xi, yi, zi) -> let x = getCoord vi xi
                                              let y = getCoord vi yi
                                              let z = getCoord vi zi
                                              Some(Point.mkPoint x y z)
        let vNormCalc (l:Vector list) = List.fold (fun acc elem -> acc + elem) (List.head l) l
                                        |> normalise
        let addToMap k v m = match Map.tryFind k m with
                             | None -> Map.add k [v] m
                             | Some(l) -> Map.add k ((v)::l) m
        let mapVtoNorms () = let s = System.Diagnostics.Stopwatch.StartNew()
                             let faceNum = faceCount plyList 
                             let rec buildMap m i = 
                                 if i = faceNum then m else
                                 match faces.[i] with
                                 | [|a;b;c|] -> 
                                       let norm = let (xi, yi, zi) = (XYZIndexes plyList).Value
                                                  let (xa, ya, za) = (getCoord a xi, getCoord a yi, getCoord a zi) 
                                                  let (xb, yb, zb) = (getCoord b xi, getCoord b yi, getCoord b zi)
                                                  let (xc, yc, zc) = (getCoord c xi, getCoord c yi, getCoord c zi)
                                                  let u = mkVector1 (xb-xa,yb-ya, zb-za) //b-a
                                                  let v = mkVector1 (xc-xa,yc-ya,zc-za) //c-a
                                                  (crossProduct u v) |> normalise
                                       let mapA = addToMap a norm m //Map vertex a to this face's normal vector
                                       let mapB = addToMap b norm mapA
                                       let mapC = addToMap c norm mapB
                                       buildMap mapC (i+1)
                                 | [||] -> m //This should not happen
                                 | _ -> failwith "Not a triangle mesh"
                             let m = buildMap Map.empty 0
                             s.Stop() ; printf "Built map in %f seconds\n" s.Elapsed.TotalSeconds
                             m
        let m = if (normIndexes plyList).IsNone && smooth then mapVtoNorms () else Map.empty
        let vNorm a b c = match normIndexes plyList with
                          | None -> let na = vNormCalc (Map.find a m)
                                    let nb = vNormCalc (Map.find b m)
                                    let nc = vNormCalc (Map.find c m)
                                    Some(na,nb,nc)
                          | Some(nx, ny, nz) -> 
                                    let na =  mkVector (getCoord a nx) (getCoord a ny) (getCoord a nz)
                                    let nb =  mkVector (getCoord b nx) (getCoord b ny) (getCoord b nz)
                                    let nc = mkVector (getCoord c nx) (getCoord c ny) (getCoord c nz)
                                    Some(na,nb,nc)
        let makeTriangle a b c n = 
                    let p1,p2,p3 = (vertex a).Value, (vertex b).Value, (vertex c).Value
                    let uv = if (uvCoords a).IsNone then None else
                             Some((uvCoords a).Value, (uvCoords b).Value, (uvCoords c).Value)
                    new Triangle (p1, p2, p3, texture, uv, n)
        let triangles =  let c = faceCount plyList
                         let q1, q2 = c / 4, c / 2
                         let q3 = q2+q1
                         let s = System.Diagnostics.Stopwatch.StartNew()
                         let makeTriangles bot top =
                                 let rec iter i shapes =
                                         if i = c then shapes else
                                         match faces.[i] with
                                         | [|a;b;c|] ->  if i = top then shapes
                                                         else if smooth 
                                                              then let norms = vNorm a b c
                                                                   iter (i+1) (makeTriangle a b c norms ::shapes) 
                                                              else iter (i+1) (makeTriangle a b c None ::shapes)              
                                         | [||] -> shapes //This should not happen
                                         | _ -> failwith "Not a triangle mesh"
                                 iter bot [] 
                         let tasks = [async {return makeTriangles 0 q1}
                                      async {return makeTriangles q1 q2}
                                      async {return makeTriangles q2 q3}
                                      async {return makeTriangles q3 c}]  
                         let t = Async.RunSynchronously (Async.Parallel tasks) |> List.concat
                         s.Stop() ; printf "Built triangles in %f seconds\n" s.Elapsed.TotalSeconds
                         TmKdtree.mkTmKdtree t (bBoxFromList (List.map(fun x -> x:> Shape) t)).Value
        let bBawx = let box = (TmKdtree.getBox triangles)
                    match box with
                    |Some b -> b
                    |None -> failwith"expected the triangle mesh to have a bounding box"
        interface Shape with 
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = TmKdtree.getBox triangles
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = match TmKdtree.traverse triangles ray bBawx with
                                              |Some x -> x
                                              |None -> None