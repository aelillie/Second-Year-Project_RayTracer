module TriangleMesh

open PlyParse
type Shape = Shape.Shape
type TriangleMesh =
  | TM of Shape list

let collectFaces = function
 |Face(x) -> [x]
 |_ -> []

let collectVertices = function
 |Vertex(x,y,z) -> [x,y,z]
 |_ -> []

let mkPointFromIndex p i list =
    let (x,y,z) = List.item i list
    Point.mkPoint (x - Point.getX p) (y - Point.getY p) (z - Point.getZ p)
    

let mkTriangleMesh p (plyList:Ply list) =
    let vertexList = plyList |> List.collect collectVertices
    let faceList = plyList |> List.collect collectFaces
    
    let rec makeTriangles vList fList = 
        match fList with
         |[] -> []
         |l::fList' ->  
                        let p1 = mkPointFromIndex p (List.item 0 l) vList
                        let p2 = mkPointFromIndex p (List.item 1 l) vList
                        let p3 = mkPointFromIndex p (List.item 2 l) vList
                        
                        (Shape.mkTriangle p1 p2 p3 (Material.mkMaterial (Colour.mkColour 1.0 1.0 1.0) 0.0))::makeTriangles vList fList'
    
    let tri = makeTriangles vertexList faceList

    tri

