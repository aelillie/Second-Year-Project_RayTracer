open Vector
open Ray
open Shape
open Material
open Colour
open Camera
open Drawing
open Light
open System.Drawing
open Point
open TracerTest

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()
    TracerTest.doTest()  
    (*let light = mkLight (mkPoint 0.0 2.0 4.0) (fromColor Color.White) 1.0 
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 (mkMaterial (fromColor Color.Blue) 0.0)
    let scene = Scene.mkScene [sphere] [light] ambientLight camera 0
    Scene.renderToFile scene "fuck dat" |> ignore
    *)

    System.Console.WriteLine "Saved image!"

    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
