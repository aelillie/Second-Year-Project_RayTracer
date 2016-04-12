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
    PointTest.doTest()
    VectorTest.doTest()
    ExprParseTest.doTest()
    ExprToPolyTest.doTest()
    TracerTest.doTest()  

    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
