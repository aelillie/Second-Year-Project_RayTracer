module Program
open Vector
open Ray
open Material
open Colour
//open BoundingBox 
open Camera
open Drawing
open Texture
open Light
open System.Drawing
open System
open Point
open PlyParse
open System.IO
open TestSuite

//let testAll =
//    PointTest.doTest ()
//    VectorTest.doTest ()
//    ExprParseTest.doTest ()
//    ExprToPolyTest.doTest ()
//    BoundingBoxTest.doTest ()
//    TransformationTest.doTest ()
//    PLYParserTest.doTest ()


let renderAll toScreen =

//  Shapes.render toScreen
//  printf "Shapes rendered\n"
//  AffineTransformations.render toScreen
//  printf "AffineTransformations rendered\n"
//  ImplicitSurfaces.render toScreen
//  printf "ImplicitSurfaces rendered\n"
//  Meshes.render toScreen
//  printf "Meshes rendered\n"
  Texture.render toScreen
  printf "Texture rendered\n"
//  Light.render toScreen
//  printf "Light rendered\n"
//  CSG.render toScreen
//  printf "CSG rendered\n"


[<EntryPoint>]
let main argv = 
//    testAll
    renderAll false
    Console.ReadKey() |> ignore
    0 // return an integer exit code