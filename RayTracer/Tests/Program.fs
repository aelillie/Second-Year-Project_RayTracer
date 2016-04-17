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

    let writer = new StreamWriter (@"C:\Users\i5-4670K\Documents\test.rtf", false, System.Text.Encoding.UTF8,512)

    writer.AutoFlush <- true


    let filepath = @"C:\Users\i5-4670K\Documents\urn2.ply.txt"


    let ply = PlyParse.parsePly filepath


    List.iter (fun x -> PlyParse.print x writer) ply


        
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
