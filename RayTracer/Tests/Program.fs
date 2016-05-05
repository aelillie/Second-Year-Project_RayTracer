﻿module Program
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
 //   BoundingBoxTest.doTest ()
//    TransformationTest.doTest ()

let renderAll toScreen =

//  Shapes.render toScreen
 // printf "Shapes rendered"
 // AffineTransformations.render toScreen
 // printf "AffineTransformations rendered"
//  ImplicitSurfaces.render toScreen
//  printf "ImplicitSurfaces rendered"
//  Meshes.render toScreen
//  printf "Meshes rendered"
  TextureTest.render toScreen
  printf "Texture rendered"
//  Light.render toScreen
//  printf "Light rendered"
//  CSG.render toScreen
//  printf "CSG rendered"


[<EntryPoint>]
let main argv = 
    renderAll false
//    renderAll false
    Console.ReadKey() |> ignore
    0 // return an integer exit code