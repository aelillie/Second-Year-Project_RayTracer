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

    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    let rays = mkRays camera
    let mat = mkMaterial (mkColour 0.5 0.5 0.5) 1.0
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 mat
    let viewPlane = List.map(fun (r, (_,_)) -> sphereHit r sphere) rays
    let coords = List.map (fun (r,p) -> p) rays
    mkPicture coords viewPlane 500 500 |> ignore

    System.Console.WriteLine "Saved image!"
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
