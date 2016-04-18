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
open PlyParse
open System.IO

[<EntryPoint>]
let main argv = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    (*PointTest.doTest()
    VectorTest.doTest()
    ExprParseTest.doTest()
    ExprToPolyTest.doTest()*)
    TracerTest.doTest()  
    //TransformationTest.doTest()


    stopWatch.Stop()
    printfn "Elapsed time (ms): %f" stopWatch.Elapsed.TotalMilliseconds
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
