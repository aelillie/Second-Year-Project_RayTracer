open Vector
open Ray
open Shape
open Material
open Colour
open Camera
open Drawing
open Light
open System.Drawing
open Point

[<EntryPoint>]
let main argv = 
//    PointTest.doTest()
//    VectorTest.doTest()
//    ExprParseTest.doTest()
//    ExprToPolyTest.doTest()

    let light = mkLight (mkPoint -3.0 3.0 3.0) (fromColor Color.White) 1.0 
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 (mkMaterial (fromColor Color.Purple) 0.0)

    let rays = mkRays camera
    let viewPlane = List.map(fun (r, (x,y)) -> (x,y, r, sphereHit r sphere)) rays
   


    let calculateShadowRay (p:Point) (rd:Vector) (l:Light) (x:float) :Ray = 
    
        let p' = Vector.multScalar rd x |> Point.move p 

        let sr = Point.direction p' (Light.getPoint l)

        Ray.mkRay p' 1.0 sr
    
        


    let x (d:Vector) (nV:Vector) : Vector = 
        let k = (d * nV)
        if (d * nV) > 0.0 then (-1.0 * nV) else nV

    let checkT t p o =
        let k = Point.distance p o |> Vector.magnitude
        t > k

    let testScene (R(p,t,d)) = function
        
        
        |None -> (System.Drawing.Color.White)

        |Some (t,nV,c) -> let nV' = x d nV
                          let p' = Point.move p (Vector.multScalar d t) 
                          let i = Light.getAmbientI ambientLight
                          let sr = calculateShadowRay p' nV' light 0.0001 
                          match sphereHit sr sphere with
                           |None    -> let i' = i + (Light.calculateI nV' (Ray.getD sr) (Light.getLightI light))
                                       Colour.toColor (Colour.scaleColour c i')

                           |Some(t',_,_) -> let o = Camera.getPoint camera
                                            if checkT t' p o 
                                            then 
                                             let i' = i + (Light.calculateI nV' (Ray.getD sr) (Light.getLightI light))
                                             Colour.toColor (Colour.scaleColour c i')
                                            else
                                             let c' = Colour.scaleColour c i
                                             Colour.toColor c'
                                       
   
        

    let k = List.map (fun (x,y,r,v) -> (x,y, testScene r v) ) viewPlane

    mkPicture k 500 500 |> ignore

    System.Console.WriteLine "Saved image!"
    System.Console.WriteLine "Press any key to close..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
