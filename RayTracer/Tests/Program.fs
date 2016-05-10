module Program
open Vector
open Ray
open Material
open Colour
//open BoundingBox 
open Camera
open Drawing
open Light
open System.Drawing
open System
open Point
open PlyParse
open System.IO
open TestSuite

let testAll = ()
//    PointTest.doTest ()
//    VectorTest.doTest ()
//    ExprParseTest.doTest ()
//    ExprToPolyTest.doTest ()
//    BoundingBoxTest.doTest ()
//    TransformationTest.doTest ()
//    //ImplicitSurfacesTest.doTest ()

let renderAll toScreen =

//  Shapes.render toScreen
//  printf "Shapes rendered"
//  AffineTransformations.render toScreen
//  printf "AffineTransformations rendered"
    ImplicitSurfacesTest.render toScreen
    printf "ImplicitSurfaces rendered"
//  //Meshes.render toScreen
//  printf "Meshes rendered"
//  //Texture.render toScreen
//  printf "Texture rendered"
//  Light.render toScreen
//  printf "Light rendered"
//  CSG.render toScreen
//  printf "CSG rendered"


[<EntryPoint>]
let main argv = 
//    testAll
    renderAll false
    0 // return an integer exit code
