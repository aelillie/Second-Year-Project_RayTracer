namespace Shapes

open Ray
open Material
open Vector
open Point
open Transformation
open BasicShape
open TransformedShape
open PlyParse

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

    type TriangleMesh (plyList, texture) = 
        let triangles = 
                let s = System.Diagnostics.Stopwatch.StartNew()
                s.Start()
                let mkVertex i vertices =
                        let vertex = List.item i vertices
                        match XYZIndexes plyList with 
                        | None -> failwith "No x y z coordinates in ply file" 
                        | Some(xi, yi, zi) -> let x = List.item xi vertex
                                              let y = List.item yi vertex
                                              let z = List.item zi vertex
                                              let p = Point.mkPoint x y z
                                              match textureIndexes plyList with
                                              | None -> (p, [])
                                              | Some(ui, vi) -> let u = List.item ui vertex
                                                                let v = List.item vi vertex
                                                                (p, [(u,v)])
                let num = faceCount plyList
                let q1, q2 = num / 4, num / 2
                let q3, q4 = q2+q1, num

                let makeTriangles bot top =
                        let rec make i shapes vertices faces =
                                if i = num then shapes else
                                match List.item i faces with
                                | [a;b;c] ->  if i = top then shapes
                                              else let (p1,l1) = mkVertex a vertices
                                                   let (p2,l2) = mkVertex b vertices
                                                   let (p3,l3) = mkVertex c vertices
                                                   
                                                   make (i+1) (new Triangle (p1, p2, p3, texture, (l1@l2@l3)) :> Shape::shapes) vertices faces
                                | [] -> shapes //This should not happen
                                | _ -> failwith "Not a triangle mesh"
                        make bot [] (vertices plyList) (faces plyList)  
                let tasks = [async {return makeTriangles 0 q1};
                             async {return makeTriangles q1 q2};
                             async {return makeTriangles q2 q3}
                             async {return makeTriangles q3 q4}]  
                Async.RunSynchronously (Async.Parallel tasks) |> List.concat
                

        interface Shape with 
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = bBoxFromList triangles
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                                    let min = List.map(fun (x:Shape) -> x.hit ray) triangles |> List.choose id
                                    match min with
                                    |[] -> None
                                    |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)



