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
open ExprParse
open ExprToPoly

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
    let expr  = parseStr "-(15.0/5.0)*x^1+y^2 +(20.0/4.0)*z^3"
    let expr2 = parseStr "-(x/4)*x^2 + 30*y^1"
    let simpleExpr2 = exprToSimpleExpr expr2
    let simpleExpr = exprToSimpleExpr expr
    let s = ppSimpleExpr simpleExpr
    let s2 = ppSimpleExpr simpleExpr2
    printf "%s" s2
    printf "%s" s 
    renderAll false
    0 // return an integer exit code
