﻿module Scene

open Camera
open Light
open Ray
open Point
open Vector
open Drawing
open Colour
open Shapes.Shape
open Texture
open Material

type BoundingBox = Shapes.BasicShape.BoundingBox
type Shape = Shapes.BasicShape.Shape

type Scene =
  | S of Shape list * Light list * AmbientLight * Camera * int

let mkScene shapes lights ambientLight camera reflection = S(shapes, lights, ambientLight, camera, reflection)
let epsilon = 0.0001


let plusTripleFloat (a,b,c) (x,y,z) : (float * float * float) =
 (a+x,b+y,c+z)
//Sorts through a list of options returning the value of Some in a list.
let sort = function
    |None -> [] 
    |Some(x) -> [x]

//Creates a ray with direction towards a specific light.
let mkShadowRay (p:Point) (l:Light)  :Ray = 
    let sr = Point.direction p (Light.getPoint l)
    Ray.mkRay p sr


//Function for finding if shape is hit

let hit r (b:BoundingBox option) (s:Shape) = 
    match b with
     |None -> s.hit r
     |Some bBox -> match bBox.hit r with
                    |Some _ -> s.hit r
                    |None -> None


//Returns true if a point is shaded from light for a given lightsource or not
let rec isShaded (r:Ray) (xs:Shape list) (bx) (l:Light) =
        match xs with
        |[] -> false
        | s::xs'  ->    let b = List.head bx                           //Check if shape is hit with ray towards a light
                        match hit r b s with
                        |None   -> isShaded r xs' (bx.Tail) l        //Check all possible shapes.
                        |Some(t',_,_) -> let tlight = Point.distance (Ray.getP r) (Light.getPoint l) |> Vector.magnitude
                                         if t' > tlight     //Remember to check if shape is behind the lightsource.
                                         then isShaded r xs' (bx.Tail) l 
                                         else true


// recursively casts rays to determine the color a given ray should register
let renderScene (S(shapes, lights, ambi, cam, n)) =
    let rays = List.toArray (mkRays cam)  //Create rays from camera
    let maxRefl = n
    let bBoxes = List.map (fun (x:Shape) -> x.getBounding()) shapes
    //Cast a single ray into the scene
    let rec castRay (ray:Ray) reflNumber = 
        let hitResults = List.map2 (fun (x:Shape) (b:BoundingBox option) ->   hit ray b x) shapes bBoxes //Check which shape are hit by a fire
        
        let intersections = List.choose id hitResults //Sort all None options out.

        match intersections with 
            | [] -> None
            | _  -> let (t,nV,m) = List.minBy (fun (t,_,_) -> t ) intersections //find intersection with minimum distance
                    let nV' = if (Ray.getD ray) * nV > 0.0 then (-1.0 * nV) else nV //Check if normalVector has to be inversed
                    let i = Light.getAmbientI ambi

                    //Moved point to the surface of the shape hit.
                    let p = Point.move (Ray.getP ray) (Vector.multScalar (Ray.getD ray) t)  
                    let p' = Point.move p (Vector.multScalar nV' epsilon)

                    let srays = List.map (fun x -> (x, mkShadowRay p' x )) lights //Create rays towards each lightsource from point.
                    //Filter all shadowRays that don't hit out
                    let sraysHit = List.filter (fun (l, r) -> not (isShaded r shapes bBoxes l )) srays

                    //Okay here calculate intensity for each colour value. 
                    let lightColourValue = let ix = List.map (fun (l,r) -> (Light.calculateI nV' (Ray.getD r))) sraysHit //Angles calculated
                                           let cx = List.map2 (fun (l,r) (i)  -> Light.getColourI l i) sraysHit ix //RGB Color value calculated from light Intensity and angle
                                           List.fold (fun acc x ->  plusTripleFloat acc x) (Light.getAmbientI ambi) cx//Folding colours together
                    //Scale color from the intensity of each colour
                    let c' = Colour.scaleColour lightColourValue (Material.getColour m)
                    //Create the reflection ray with respect to ingoing ray and the normal vector.
                    let x2 = ((Ray.getD ray) * nV') * 2.0
                    let reflectDir = Vector.normalise ((Ray.getD ray) - (x2 * nV'))
                    let reflRay = Ray.mkRay p' reflectDir
                    if Material.getReflection m > 0.0 
                    then    //If reflective material then fire out rays recursively and check with color is returned.
                        match reflNumber with
                         | _ when reflNumber < maxRefl -> let reflV = Material.getReflection m
                                                          let res = castRay reflRay (reflNumber+1)
                                                          match res with
                                                           |None -> Some c'
                                                           |Some (cRef) -> Some (Colour.merge reflV cRef c' )  
                         | _ -> Some c'
                    else Some c'    //Else return own color.    
    //Mapping the rays to colours for each pixel.
    let pixelplane = Array.Parallel.map (fun (r, (x,y)) ->(x,y, castRay r 0)) rays
    //Map from colour to color.
    List.map (fun (x,y,c) -> x,y, sort c |> toColorFromList) (List.ofArray pixelplane)





let renderToFile ((S(shapes, lights, ambi, cam, n)) as scene) (filename:string) = 
    let res = renderScene scene
    let (px, py) = Camera.getRes cam
    let bitmap = Drawing.mkPicture res px py
    //Saves the file
    bitmap.Save(filename)



let renderToScreen ((S(shapes, lights, ambi, cam, n)) as scene) =
    let res = renderScene scene
    let (px, py) = Camera.getRes cam
    //Create a bitmap using the values rendered from the scene
    let bitmap = Drawing.mkPicture res px py
    //Make window for showing image
    let window = Drawing.mkWindow bitmap
    System.Windows.Forms.Application.Run window 




