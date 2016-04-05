open Point
open Vector
open Ray
open Shape
open Material
open Colour
open Camera
open Drawing

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()

    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 1.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    let rays = mkRays camera
    let mat = mkMaterial (mkColour 0.5 0.5 0.5) 1.0
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 mat
    let viewPlane = List.map(fun (r, (_,_)) -> sphereHit r sphere) rays
    let coords = List.map (fun (r,(x,y)) -> x,y) rays
    let light = Light.mkLight (mkPoint 0.0 0.0 3.0) (fromColor System.Drawing.Color.White) 1.0

    let testScene (R(p,t,d)) = function
        |None -> (System.Drawing.Color.White)
        |Some (t,nV,c) -> let p' = Point.move p (Vector.multScalar d t) 
                          let sr = Light.getShadowRay p' nV light 0.0001 
                          match sphereHit sr sphere with
                           _ -> let c' = Light.scaleColour (Colour.getRGB c) nV (Ray.getD sr)
                                (Colour.toColor c' )
    
    let k = List.map2 (fun (x,r) v -> testScene x v ) rays viewPlane

    let v = List.map2 (fun (x,y) r -> (x,y,r)) coords k

    mkPicture v 500 500 |> ignore

    System.Console.WriteLine "Saved image!"
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
