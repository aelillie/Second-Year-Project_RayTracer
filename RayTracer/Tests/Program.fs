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
    (*PointTest.doTest()
    VectorTest.doTest()
    ExprParseTest.doTest()
    ExprToPolyTest.doTest()*)
    //TracerTest.doTest()  

    let file = @"C:\Users\i5-4670K\Documents\urn2.ply.txt"


    
    PlyParse.parsePly file |> ignore
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
