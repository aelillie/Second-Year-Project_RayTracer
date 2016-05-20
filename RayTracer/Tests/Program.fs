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
open ImplicitTest

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
//  printf "shapes rendered"
//  AffineTransformations.render toScreen
//  printf "AffineTransformations rendered"
    ImplicitSurfaces.render toScreen
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
//
//    let rs1 = "(" + (string 1.5) + "^2" + " + " + (string 0.5) + "^2)"
//    let rs2 = "(" + (string 1.5) + "^2" + " +- " + (string 0.5) + "^2)"
//    let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 + -2*" + rs1 + "*x^2"
//    let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
//    let sz = "z^4 + -2*" + rs1 + "*z^2"
//    let sc = rs2 + "^2"
//    let eqn = sx + " + " + sy + " + " + sz + " + " + sc
//    printf "%s\n" eqn
//    let expr = parseStr eqn
//    printf "\n%s" (ppExpr expr)
//    testall
//    ImplicitTest.doTest() 
    renderAll false
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
