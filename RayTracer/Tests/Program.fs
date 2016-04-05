open Point
open Vector
open Ray
open Shape
open Material
open Colour
open Camera
open Drawing
open Light

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()

    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 1.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    let ambientLight = Light.mkAmbientLight (fromColor System.Drawing.Color.White) 0.1
    let mat = mkMaterial (mkColour 0.5 0.5 0.5) 1.0
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 mat
    let light = Light.mkLight (mkPoint 0.0 1.0 3.0) (fromColor System.Drawing.Color.White) 1.0


    let rays = mkRays camera
    let viewPlane = List.map(fun (r, (x,y)) -> (x,y, r, sphereHit r sphere)) rays
   
    let getShadowRay (p:Point) (rd:Vector) (l:Light) (x:float) :Ray = 
    
        let p' = Vector.multScalar rd x |> Point.move p 

        let sr = Point.direction p' (Light.getPoint l)

        Ray.mkRay p' 1.0 sr


    let testScene (R(p,t,d)) = function
        |None -> (System.Drawing.Color.White)

        |Some (t,nV,c) -> let p' = Point.move p (Vector.multScalar d t) 
                          let i = Light.getAmbientI ambientLight
                          let sr = getShadowRay p' nV light 0.0001 
                          match sphereHit sr sphere with
                           |None    -> let i' = i + (Light.calculateI nV (Ray.getD sr) (Light.getLightI light))
                                       Colour.toColor (Colour.scaleColour c i')
                           |Some(_) -> let c' = Colour.scaleColour c i
                                       Colour.toColor c'
                                       
    
    let k = List.map (fun (x,y,r,v) -> (x,y, testScene r v) ) viewPlane

    mkPicture k 500 500 |> ignore

    System.Console.WriteLine "Saved image!"
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
