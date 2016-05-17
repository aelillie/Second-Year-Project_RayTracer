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
    

    type Box(low,high,front,back,top,bottom,left,right) = 
        let rects = 
                let width = System.Math.Abs(Point.getX high - Point.getX low)
                let height = System.Math.Abs(Point.getY high - Point.getY low)
                let depth = System.Math.Abs(Point.getZ high - Point.getZ low)
                let az = System.Math.Min(Point.getZ high, Point.getZ low)

                let frontT = translate (Point.getX low) (Point.getY low) az 
                let backT =   mergeTransformations [frontT;translate 0.0 0.0 depth]
                let bottomT = mergeTransformations [rotateX (pi/2.0);frontT]
                let topT =    mergeTransformations [bottomT;translate 0.0 height 0.0]
                let leftT =   mergeTransformations [rotateY (-(pi/2.0));frontT]
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
                |> List.map2 (fun t s -> new TransformedShape (s, t)) transformations
                |> List.map (fun s -> s:>Shape)

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
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = this.hit ray 



    type SolidCylinder(c,r,h,t,top,bottom) = 
        let cyl = 
            let cyl' = (new HollowCylinder (c, r, h, t)) :> Shape
            let botDisc = new Disc (c, r, bottom)
            let topDisc = new Disc (c, r, top)

            let transTop = mergeTransformations [rotateX (-(pi/2.0));translate 0.0 (h/2.0) 0.0]
            let transBot = mergeTransformations [rotateX ((pi/2.0));translate 0.0 (-h/2.0) 0.0]
            let top' = (transform topDisc transTop) :> Shape
            let bot' = (transform botDisc transBot) :> Shape 
            [cyl';bot';top']

        interface Shape with
            member this.isInside p = let (x,y,z) = Point.getCoord p
                                     let (cx, cy, cz) = Point.getCoord c
                                     let lowH, highH = cy-(h/2.0), cy+(h/2.0)
                                     ((x-cx)**2.0 + (z-cz)**2.0) < r**2.0
                                     && lowH < y && y < highH
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                            let hits = List.map(fun (x:Shape) -> x.hit ray) cyl
                            let min = hits |> List.choose id
                            match min with
                            |[] -> None
                            |_ ->  Some(List.minBy (fun (di, nV, mat) -> di) min) 

    type TriangleMesh (plyList, texture) = 
        let triangles = 

                let vertexList = 
                    plyList |> 
                    List.collect (fun x -> match x with
                                           | Vertex(floatList) -> [floatList]
                                           | _ -> [])

                let mkVertex i vertices =
                    let vertex = List.item i vertices
                    let x = List.item 0 vertex
                    let y = List.item 1 vertex
                    let z = List.item 2 vertex
                    let u = List.item 6 vertex
                    let v = List.item 7 vertex
                    ((Point.mkPoint x y z), [(u,v)])
//                    match textureIndexes plyList with
//                    | None -> ((Point.mkPoint x y z), [])
//                    | Some(ui, vi) -> 
//                        let u = List.item ui vertex
//                        let v = List.item vi vertex
//                        ((Point.mkPoint x y z), [(u,v)])
            
                let rec makeTriangles shapes vertices = function
                     | Face([a;b;c])::rest->  
                                    let (p1,l1) = mkVertex a vertices
                                    let (p2,l2) = mkVertex b vertices
                                    let (p3,l3) = mkVertex c vertices
                                    
                                    makeTriangles (new Triangle (p1, p2, p3, texture, (l1@l2@l3)) :> Shape::shapes) vertices rest
                     | _::rest -> makeTriangles shapes vertices rest
                     | [] -> shapes
                printf "Triangles constructed"
                makeTriangles [] vertexList plyList

        interface Shape with 
            member this.isInside p = failwith "Not implemented"
            member this.getBounding () = 
                                    let shapeX = List.map(fun x -> x:> Shape) triangles
                                    let sbbox = List.map (fun (c:Shape) -> c.getBounding().Value) shapeX

                                    let bL = List.map (fun (b:BasicShape.BoundingBox) -> b.getL) sbbox
                                    let bH = List.map (fun (b:BasicShape.BoundingBox) -> b.getH) sbbox

                                    let minX = List.minBy (fun x -> Point.getX x) bL
                                    let minY = List.minBy (fun x -> Point.getY x) bL
                                    let minZ = List.minBy (fun x -> Point.getZ x) bL

                                    let maxX = List.maxBy (fun x -> Point.getX x) bH
                                    let maxY = List.maxBy (fun x -> Point.getY x) bH
                                    let maxZ = List.maxBy (fun x -> Point.getZ x) bH
//                                    let max = List.maxBy (fun x -> (Point.getX x, Point.getY x, Point.getZ x)) (List.map (fun (b:BasicShape.BoundingBox) -> b.getH) sbbox)
//                                    let min = List.minBy (fun x -> (Point.getX x, Point.getY x, Point.getZ x)) (List.map (fun (b:BasicShape.BoundingBox) -> b.getL) sbbox) 
                                    Some {p1=(mkPoint (Point.getX minX - epsilon) (Point.getY minY - epsilon) (Point.getZ minZ - epsilon) ) 
                                     ; p2=(mkPoint (Point.getX maxX + epsilon) (Point.getY maxY + epsilon) (Point.getZ maxZ + epsilon) )}
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                                    let min = List.map(fun (x:Shape) -> x.hit ray) triangles |> List.choose id
                                    match min with
                                    |[] -> None
                                    |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)



