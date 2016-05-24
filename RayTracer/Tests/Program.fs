module Program
open TracerTestSuite
open TestSuite
open System.IO
open System

let testAll () =
    PointTest.doTest ()
    VectorTest.doTest ()
    ExprParseTest.doTest ()
//    ExprToPolyTest.doTest ()
    BoundingBoxTest.doTest ()
    TransformationTest.doTest ()
    PLYParserTest.doTest ()


let renderAll toScreen =
  Shapes.render toScreen
  printf "Shapes rendered\n"
  AffineTransformations.render toScreen
  printf "AffineTransformations rendered\n"
//  ImplicitSurfaces.render toScreen
//  printf "ImplicitSurfaces rendered\n"
//  Meshes.render toScreen
//  printf "Meshes rendered\n"
  Texture.render toScreen
  printf "Texture rendered\n"
  Light.render toScreen
  printf "Light rendered\n"
  CSG.render toScreen
  printf "CSG rendered\n"


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
