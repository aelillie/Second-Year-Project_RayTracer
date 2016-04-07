module Scene

open Camera
open Shape
open Light
open Tracer
open Ray
open Point
open Vector
open Drawing

type Scene =
  | S of Shape list * Light list * AmbientLight * Camera * int

let mkScene shapes lights ambientLight camera reflection = S(shapes, lights, ambientLight, camera, reflection)

// recursively casts rays to determine the color a given ray should register
let renderToFile (S(shapes, lights, ambi, cam, n)) (filename:string) =
    let rays = mkRays cam


    let rec castRay (ray:Ray) n = 
        let intersects = List.map (fun x -> Shape.hit ray x) shapes 
        
        let calculateShadowRay (p:Point) (rd:Vector) (l:Light) (x:float) :Ray = 
            let p' = Vector.multScalar rd x |> Point.move p 
            let sr = Point.direction p' (Light.getPoint l)
            Ray.mkRay p' 1.0 sr
        
        let lightIntensity (r:Ray) (s:Shape) (l:Light) nV =
             match hit ray s with
             |None    -> let i = (Light.calculateI nV (Ray.getD r) (Light.getLightI l))
                         i
             |Some(t',_,_) -> let o = Camera.getPoint cam
                              let tlight = Point.distance (Ray.getP r) o |> Vector.magnitude  
                              if t' < tlight 
                              then 
                                let i =(Light.calculateI nV (Ray.getD r) (Light.getLightI l))
                                i
                              else
                                0.0

        let sort = function
            |None -> [] 
            |Some(t,nV,c) -> [(t,nV,c)]


        let intersects = List.collect (fun x -> sort x) intersects

        match intersects with 
            | [] -> System.Drawing.Color.White
            | _  -> let (t,nV,c) = List.minBy (fun (t,_,_) -> t ) intersects
                    let nV' = if (Ray.getD ray) * nV > 0.0 then (-1.0 * nV) else nV
                    let i = Light.getAmbientI ambi
                    let p = Point.move (Ray.getP ray) (Vector.multScalar (Ray.getD ray) t)
                    let srays = List.map (fun x -> (x, calculateShadowRay p nV' x 0.0001)) lights
                    let il = List.map2 (fun (l, r) s -> lightIntensity r s l nV') srays shapes
                    let i' = List.fold (fun acc x -> acc + x) 0.0 il
                    Colour.toColor (Colour.scaleColour c (i+i'))
    let pixelplane = List.map (fun (r, (x,y)) ->(x,y, castRay r n)) rays

    Drawing.mkPicture pixelplane 500 500 filename 





