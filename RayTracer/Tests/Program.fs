module Program
open Vector
open Ray
open Shape
open Material
open Colour
open BoundingBox 
open Camera
open Drawing
open Light
open System.Drawing
open System
open Point
open TracerTest
open PlyParse
open System.IO
open TestSuite


[<STAThreadAttribute>]
[<EntryPoint>]
let main argv = 
    //let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    (*PointTest.doTest()
    VectorTest.doTest()
    
    ExprParseTest.doTest()
    ExprToPolyTest.doTest()*)

    //TracerTest.doTest()  
    //TransformationTest.doTest()
    //BoundingBoxTest.doTest()
    //TracerTest.doTest() 
    AffineTransformationsTest.render false
    (*let filepath = @"C:\Users\i5-4670K\Documents\ant.ply.txt"

    let k = PlyParse.parsePly filepath

    let writer = new StreamWriter(@"C:\Users\i5-4670K\Documents\test.rtf")
    writer.AutoFlush <- true

    List.iter (fun x -> PlyParse.print x writer) k *)


    //stopWatch.Stop()
    //printfn "Elapsed time (ms): %f" stopWatch.Elapsed.TotalMilliseconds 

    //System.Console.WriteLine "Press any key to close..."
    //System.Console.ReadKey() |> ignore
    0 // return an integer exit code
