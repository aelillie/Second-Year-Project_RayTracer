// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Point
open Vector
open Ray
open Shape
open Material
open System.Drawing

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()
    let camera = Camera.mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    let res = Camera.mkRays camera
    let mat = mkMaterial (mkColour 0.5 0.5 0.5) 1.0
    let sphere = Shape.mkSphere (mkPoint 0.0 0.0 0.0) 2.0 mat
    let pixelPlane = List.map(fun x -> Shape.hit x sphere) res
    let bmp = new Bitmap(510,510)
    Drawing.mkPicture pixelPlane bmp |> ignore
   // for r in res do System.Console.WriteLine(r)
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
