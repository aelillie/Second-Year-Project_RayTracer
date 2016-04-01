// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Point
open Vector
open Ray
open Shape
open Material

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()
    let camera = Camera.mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    let res = Camera.mkRays camera
    let mat = mkMaterial (mkColour 1.0 0.0 0.10) 1.0
    let sphere = Shape.mkSphere (mkPoint 0.0 0.0 0.0) 2.0 mat
    let res2 = List.map(fun x -> Shape.hit x sphere) res
    let res3 = List.map(fun (_,x,y,c) -> (x,y,c)) res2
    Drawing.mkPicture res3 500 500 |> ignore
   // for r in res do System.Console.WriteLine(r)
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
