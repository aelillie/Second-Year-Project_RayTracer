module BoundingBoxTest

open System
open Shapes.Shape
open Point 
open Material
open System.Drawing
open Texture

let chk (name,res1,res2) =
  printf "%s %s\n" name (if res1 = res2 then "OK" else "FAILED\nActual:\n"+(string)res1+"\nExpected:\n"+(string)res2+"\n")

let epsilon = 0.00001

let sphere1 = mkSphere (mkPoint 0.5 0.0 0.0) 2.0 (mkMatTexture (mkMaterial(Colour.fromColor Color.Blue) 0.0))
let sphere2 = mkSphere (mkPoint -0.5 0.0 0.0) 2.0 (mkMatTexture (mkMaterial(Colour.fromColor Color.Blue) 0.0))

let sphere3 = mkSphere (mkPoint 2.5 0.0 0.0) 2.0 (mkMatTexture (mkMaterial(Colour.fromColor Color.Blue) 0.0))
let sphere4 = mkSphere (mkPoint -2.5 0.0 0.0) 2.0 (mkMatTexture (mkMaterial(Colour.fromColor Color.Blue) 0.0))

let unionSphere = (union sphere1 sphere2) :> Shape
let inteSphere = (intersection sphere1 sphere2) :> Shape
let inteSphere1 = (intersection sphere3 sphere4) :> Shape
let subSphere = (subtraction sphere1 sphere2) :> Shape
let groupSphere = (group sphere1 sphere2) :> Shape

let up1 = mkPoint (-2.5 - epsilon) (-2.0 - epsilon) (-2.0 - epsilon)
let up2 = mkPoint (2.5 + epsilon) (2.0 + epsilon) (2.0 + epsilon)

let ip1 = mkPoint (-1.5 - epsilon) (-2.0 - epsilon) (-2.0 - epsilon)
let ip2 = mkPoint (1.5 + epsilon) (2.0 + epsilon) (2.0 + epsilon)
let ip3 = mkPoint 0.0 0.0 0.0

let sp1 = mkPoint (-1.5 - epsilon) (-2.0 - epsilon) (-2.0 - epsilon)
let sp2 = mkPoint (2.5 + epsilon) (2.0 + epsilon) (2.0 + epsilon)

let gp1 = mkPoint (-2.5 - epsilon) (-2.0 - epsilon) (-2.0 - epsilon)
let gp2 = mkPoint (2.5 + epsilon) (2.0 + epsilon) (2.0 + epsilon)

let tests =
    ["Test sphere union", unionSphere.getBounding().ToString(), up1.ToString() + " " + up2.ToString();
     "Test sphere intersection", inteSphere.getBounding().ToString(), ip1.ToString() + " " + ip2.ToString();
     "Test sphere intersection1", inteSphere1.getBounding().ToString(), ip3.ToString() + " " + ip3.ToString(); //If they don't intersect
     "Test sphere subtraction", subSphere.getBounding().ToString(), sp1.ToString() + " " + sp2.ToString();
     "Test sphere group", groupSphere.getBounding().ToString(), gp1.ToString() + " " + gp2.ToString();
     ] 


let doTest() =
  printf "BoundingBoxTest\n"
  List.iter chk tests
