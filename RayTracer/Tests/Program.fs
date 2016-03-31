// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Point
open Vector
open Ray

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()
    let camera = Camera.mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 25 25
    let res = Camera.mkRays camera
    for r in res do System.Console.WriteLine(r)
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
