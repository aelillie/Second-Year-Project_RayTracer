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
                let backT =   mergeTransformations [translate 0.0 0.0 depth; frontT;]
                let bottomT = mergeTransformations [frontT; rotateX (pi/2.0)]
                let topT =    mergeTransformations [translate 0.0 height 0.0 ; bottomT; ]
                let leftT =   mergeTransformations [frontT; rotateY (-(pi/2.0))]
                let rightT =  mergeTransformations [translate width 0.0 0.0; leftT;]

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
            member this.isInside p = let (x,y,z) = Point.getCoord p
                                     let (lx,ly,lz) = Point.getCoord low
                                     let (hx,hy,hz) = Point.getCoord high
                                     lx < x && x < hx && ly < y && y < hy && lz < z && z < hz 
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = this.hit ray 



    type SolidCylinder(c,r,h,t,top,bottom) = 
        let cyl = 
            let cyl' = (new HollowCylinder (c, r, h, t)) :> Shape
            let botDisc = new Disc (c, r, bottom)
            let topDisc = new Disc (c, r, top)

            let transTop = mergeTransformations [translate 0.0 (h/2.0) 0.0; rotateX (-(pi/2.0))]
            let transBot = mergeTransformations [translate 0.0 (-h/2.0) 0.0; rotateX ((pi/2.0)) ]
            let top' = (transform topDisc transTop) :> Shape
            let bot' = (transform botDisc transBot) :> Shape 
            [cyl';bot';top']

        interface Shape with
            member this.isInside p = failwith "Not Done"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                            let hits = List.map(fun (x:Shape) -> x.hit ray) cyl
                            let min = hits |> List.choose id
                            match min with
                            |[] -> None
                            |_ ->  Some(List.minBy (fun (di, nV, mat) -> di) min) 

    type TriangleMesh (p,plyList) = 
        let rects = 
                let collectFaces = function
                 |Face(x) -> [x]
                 |_ -> []

                let collectVertices = function
                 |Vertex(floatList) -> [floatList]
                 |_ -> []

                let mkPointFromIndex p i list =
                    let x = List.item 0 (List.item i list)
                    let y = List.item 1 (List.item i list)
                    let z = List.item 2 (List.item i list)
                    Point.mkPoint (x - Point.getX p) (y - Point.getY p) (z - Point.getZ p)
                let rnd = System.Random()  
        
                let vertexList = plyList |> List.collect collectVertices
                let faceList = plyList |> List.collect collectFaces
    
                let rec makeTriangles vList fList = 
                    match fList with
                     |[] -> []
                     |l::fList' ->  
                                    let p1 = mkPointFromIndex p (List.item 0 l) vList
                                    let p2 = mkPointFromIndex p (List.item 1 l) vList
                                    let p3 = mkPointFromIndex p (List.item 2 l) vList
                                    new Triangle (p1, p2, p3, (Material.mkMaterial(Colour.fromColor System.Drawing.Color.Gray) 0.0))::makeTriangles vList fList'
    
                makeTriangles vertexList faceList |> List.map (fun x -> x:> Shape)

        interface Shape with 
            member this.isInside p = failwith "NOT FUCKING DONE"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                                    let min = List.map(fun (x:Shape) -> x.hit ray) rects |> List.choose id
                                    match min with
                                    |[] -> None
                                    |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)



