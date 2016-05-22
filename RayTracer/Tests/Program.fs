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
open ExprParse
open ExprToPoly
open ImplicitTest

let testAll =()
//    PointTest.doTest ()
//    VectorTest.doTest ()
//    ExprParseTest.doTest ()
//    ExprToPolyTest.doTest ()
////    BoundingBoxTest.doTest ()
//    TransformationTest.doTest ()


let renderAll toScreen =

//  Shapes.render toScreen
//  printf "Shapes rendered"
//  AffineTransformations.render toScreen
//  printf "AffineTransformations rendered"
  ImplicitSurfaces.render toScreen
  printf "ImplicitSurfaces rendered"
//  Meshes.render toScreen
//  printf "Meshes rendered"
//  Texture.render toScreen
//  printf "Texture rendered"
//  Light.render toScreen
//  printf "Light rendered"
//  CSG.render toScreen
//  printf "CSG rendered"


[<EntryPoint>]
let main argv = 
//
//    let s = "(x^2 + y^2 + z^2)_2 + -2"
//    let ex = parseStr s
//    printf "%s\n" (ppExpr ex)
//    let se = exprToSimpleExpr ex
//    let pp = ppSimpleExpr se
//    printf "%s" pp
//    printf "%s\n" eqn
//    let expr = parseStr eqn
//    printf "\n%s" (ppExpr expr)
//    testall
//    ImplicitTest.doTest() 
    renderAll false
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
