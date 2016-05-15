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
//    testall
    let expr  = ExprParse.parseStr "((1 + 3 * 4)/2)*x^2"
//    let expr2 = parseStr "-(x/4)*x^2 + 30*y^1"
    let ppe = ppExpr expr  
//    let simpleexpr = exprToSimpleExpr expr
//    let simpleexpr2 = exprToSimpleExpr expr2
//
//    let s = ppSimpleExpr simpleexpr
//    let s2 = ppSimpleExpr simpleexpr2
//    printf "%s" s2
//    printf "%s" s 
    renderAll false
    0 // return an integer exit code
