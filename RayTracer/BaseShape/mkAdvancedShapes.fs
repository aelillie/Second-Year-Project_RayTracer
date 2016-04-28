namespace mkShape


open Ray
open Vector
open PlyParse
open System.Drawing
open Point
open Texture
open Material
open Transformation
open EditShape
open mkBasicShapes


module mkAdvancedShapes = 
    let pi = System.Math.PI
    let pow (x, y) = System.Math.Pow(x, y)
    
    type shape2 = mkBasicShapes.shape2
    


    

    let mkBox (low : Point) (high : Point) (front : Material) (back : Material) (top : Material) (bottom : Material) (left : Material) (right : Material) :shape2  =
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
        let frontR =  mkRectangle p width height front
        let backR =   mkRectangle p width height back
        let bottomR = mkRectangle p width depth bottom
        let topR =    mkRectangle p width depth top
        let leftR =   mkRectangle p depth height left
        let rightR =  mkRectangle p depth height right

        let rectangles = [frontR;backR;bottomR;topR;leftR;rightR] 

        let rects = List.map2 (fun s t -> transform t s) transformations rectangles

        let hit (R(p,d) as ray) =   
                                    let min = List.map(fun x -> x ray) rects |> List.choose id
                                    match min with
                                     |[] -> None
                                     |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
        hit


    let mkBoxCenter front back top bottom left right = 
        mkBox (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) front back top bottom left right



    let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Material) (top : Material) (bottom : Material) = 

        let cyl = mkHollowCylinder c r h t 
        let botDisc = mkDisc c r bottom
        let topDisc = mkDisc c r top

        let transTop = mergeTransformations [translate 0.0 (h/2.0) 0.0; rotateX (-(pi/2.0))]
        let transBot = mergeTransformations [translate 0.0 (-h/2.0) 0.0; rotateX ((pi/2.0)) ]
        let top' = transform topDisc transTop
        let bot' = transform botDisc transBot  

        let hit (R(p,d) as ray) = 
                            let hits = List.map(fun x -> x ray) [cyl;top';bot']
                            let min = hits |> List.choose id
                            match min with
                            |[] -> None
                            |_ ->  Some(List.minBy (fun (di, nV, mat) -> di) min) 
        hit


    let mkSolidCylinderCenter r h t top bottom = mkSolidCylinder (mkPoint 0.0 0.0 0.0) r h t top bottom


    let mkTriangleMesh p (plyList:Ply list) : shape2 = 
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
                            (mkTriangle p1 p2 p3 (Material.mkMaterial(Colour.fromColor System.Drawing.Color.Gray) 0.0))::makeTriangles vList fList'
    
        let rects = makeTriangles vertexList faceList

        let hit (R(p,d) as ray) = 
                                    let min = List.map(fun x -> x ray) rects |> List.choose id
                                    match min with
                                    |[] -> None
                                    |_ -> Some(List.minBy (fun (di, nV, mat) -> di) min)
        hit


    
  

    
