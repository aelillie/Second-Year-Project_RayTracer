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
    let maxRefl = n

    let rec castRay (ray:Ray) reflNumber = 
        let hitResults = List.map (fun x -> Shape.hit ray x) shapes 
        
        let mkShadowRay (p:Point) (rd:Vector) (l:Light)  :Ray = 
            let sr = Point.direction p (Light.getPoint l)
            Ray.mkRay p 1.0 sr
        
        let rec isShaded (r:Ray) (xs:Shape list) (l:Light) =

             match xs with
             |[] -> false
             | s::xs'  ->
                 match hit ray s with
                 |None   -> isShaded r xs' l
                 |Some(t',_,_) -> let o = Camera.getPoint cam
                                  let tlight = Point.distance (Ray.getP r) o |> Vector.magnitude  
                                  if t' < tlight 
                                  then 
                                    isShaded r xs' l
                                  else
                                    true

        let sort = function
            |None -> [] 
            |Some(t,nV,m) -> [(t,nV,m)]


        let intersections = List.collect (fun x -> sort x) hitResults

        match intersections with 
            | [] -> Colour.mkColour 1.0 1.0 1.0
            | _  -> let (t,nV,m) = List.minBy (fun (t,_,_) -> t ) intersections
                    let nV' = if (Ray.getD ray) * nV > 0.0 then (-1.0 * nV) else nV
                    let i = Light.getAmbientI ambi
                    let p = Point.move (Ray.getP ray) (Vector.multScalar (Ray.getD ray) t)  
                    let p' = Point.move p (Vector.multScalar nV' 0.0001)
                    let srays = List.map (fun x -> (x, mkShadowRay p' nV' x )) lights
                    let i' = List.filter (fun (l, r) -> not (isShaded r shapes l)) srays
                                 |> List.map (fun (l,r) -> (Light.calculateI nV' (Ray.getD r) (Light.getLightI l)))
                                 |> List.fold (fun acc x -> acc + x) 0.0

                    let c' = (Colour.scaleColour (Material.getColour m) (i+i'))
                    let reflectDir = Ray.getD ray - 2.0 * Vector.normalise((Ray.getD ray * nV' * nV'))
                    let reflRay = Ray.mkRay p' 1.0 reflectDir
                    if Material.getReflection m > 0.0 
                    then
                        match reflNumber with
                         | _ when reflNumber < maxRefl -> let reflV = Material.getReflection m
                                                          (Colour.merge reflV c' (castRay reflRay (reflNumber+1)))
                         | _ -> Colour.scaleColour c' (1.0 - Material.getReflection m)
                    else Colour.scaleColour c' (1.0 - Material.getReflection m)     




    let pixelplane = List.map (fun (r, (x,y)) ->(x,y, castRay r n)) rays


    let pixelplane' = List.map (fun (x,y,c) -> x, y, Colour.toColor c) pixelplane

    Drawing.mkPicture pixelplane' 500 500 filename 





