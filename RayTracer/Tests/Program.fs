// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Point
open Vector
open Ray
open Shape
open Material
open System.Drawing
open Tracer

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()
    let camera = Camera.mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    let res = Camera.mkRays camera
    let mat = mkMaterial (mkColour 0.5 0.5 0.5) 1.0
    let sphere = Shape.mkSphere (mkPoint 0.0 0.0 0.0) 2.0 mat
    let pixelPlane = List.map(fun x -> Shape.hit x sphere) res
    let light = Light.mkLight (mkPoint (-1.0) 2.0 4.0) (fromColor Color.White) 1.0

    let testScene (R(x,y,p,t,d)) = function
        |None -> (x,y, Color.White)
        |Some(x,y,t',nV,(c:System.Drawing.Color)) ->  let p' = Point.move p (Vector.multScalar d t') 
                                                      let sr = Light.getShadowRay p' nV light 0.0001 
                                                      match Shape.hit sr sphere with
                                                             None -> (x,y,System.Drawing.Color.Black)
                                                            |Some(_) -> let c' = Light.scaleColour (0.5,0.5,0.5) nV (Ray.getD sr)    
                                                                        (x,y,c')


    let k = List.map2 (fun r p -> testScene r p) res pixelPlane









    let bmp = new Bitmap(510,510)
    Drawing.mkPicture k bmp |> ignore
   // for r in res do System.Console.WriteLine(r)
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
