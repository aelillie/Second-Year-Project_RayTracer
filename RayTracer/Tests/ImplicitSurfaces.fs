﻿namespace TestSuite
open System.Drawing
open Light
open Implicit
open Shapes.ImplicitShape
open Texture
open Point
open Colour
open Shapes.Shape
open Util
open Material
open Camera
open Scene
open Vector

module ImplicitSurfaces =

  let folder = "implicitSurfaces"

  let light = mkLight (mkPoint 4.0 2.0 4.0) (fromColor Color.White) 0.5
  let light2 = mkLight (mkPoint -4.0 2.0 4.0) (fromColor Color.White) 0.5
  let lights = [light; light2]
  let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in

  let sphere1 (r : float) toScreen =
    let s = mkShape (mkImplicit ("x^2 + y^2 + z^2 + -" + (string (r * r)))) (mkMaterial (fromColor Color.Aqua) 0.0)
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sphere1.png"))

  let sphere2 (r : float) toScreen =
    let s = mkShape (mkImplicit ("(x^2 + y^2 + z^2)_2 + -" + (string r))) (mkMaterial (fromColor Color.Blue) 0.0)
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sphere2.png"))

  let planeX toScreen =
    let s = mkShape (mkImplicit "x") (mkMaterial (fromColor Color.Blue) 0.0)
    let camera = mkCamera (mkPoint 1.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 16.0 16.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "planeX.png"))

  let planeY toScreen =
    let s = mkShape (mkImplicit "y")  (mkMaterial (fromColor Color.Blue) 0.0)
    let camera = mkCamera (mkPoint 0.0 1.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "planeY.png"))

  let planeZ toScreen =
    let s = mkShape (mkImplicit "z") (mkMaterial (fromColor Color.Blue) 0.0)
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "planeZ.png"))

// A torus takes two arguments, the inner and the outer radius of the torus (r and R respectively). 
//  The outer radius is the distance from the center of the torus to the center of the tube
//  The inner radius is the radius of the tube
  let torus (R : float) (r : float) toScreen =
    let s = mkShape (mkImplicit ("(((x^2 + y^2)_2 + -" + (string R) + ")^2 + z^2)_2 + -" + (string r)))  (mkMaterial (fromColor Color.Blue) 0.0)
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "torus.png"))

  let torus2 (R : float) (r : float) toScreen =
    let rs1 = "(" + (string R) + "^2" + " + " + (string r) + "^2)"
    let rs2 = "(" + (string R) + "^2" + " - " + (string r) + "^2)"
    let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*" + rs1 + "*x^2"
    let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
    let sz = "z^4 - 2*" + rs1 + "*z^2"
    let sc = rs2 + "^2"
    let eqn = sx + " + " + sy + " + " + sz + " + " + sc 
    let _ = printf "torus equation %s\n" eqn
    let s = mkShape (mkImplicit eqn) (mkMaterial (fromColor Color.Blue) 0.0)
    let camera = mkCamera (mkPoint 0.0 4.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "torus.png"))

      
  let testShape toScreen =
    let is = mkImplicit "(x + -2)^2*(x+2)^2 + (y + -2)^2(y+2)^2 + (z + -2)^2*(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z + -10(x^2 + y^2 + z^2) + 22"
    let s = mkShape is (mkMaterial (fromColor Color.Gold) 0.0)
    let camera = mkCamera (mkPoint 0.0 2.0 8.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "testShape.png"))
  
  let heart toScreen =
    let is = mkImplicit "(x^2 + (4.0/9.0)*y^2 + z^2 + -1)^3 + -x^2 * z^3 + -(9.0/80.0)*y^2*z^3"
    let s = mkShape is  (mkMaterial (fromColor Color.DarkRed) 0.0)
    let camera = mkCamera (mkPoint 0.0 3.0 1.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 -0.00000001 1.0) 2.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "heart.png"))

  let factorial x = 
    if x = 0 then 1 else
    let rec fac_aux a acc =
      if a >= x then
        a * acc
      else
        fac_aux (a + 1) (a * acc)
    fac_aux 1 x

  let comb a b = 
    let x = float (factorial a) in
    let y = float (factorial b) in
    let z = float (factorial (a - b)) in
      x / (y * z)

  let rec strSum n f : string =
    if n = 0 then
      f 0
    else
      f n + " + " + (strSum (n - 1) f)

  let chmutov degree toScreen =       
    let T x = strSum (degree / 2) (fun (k : int) -> (string (comb degree (2 * k))) + " * (" + x + "^2 + -1.0)^" + (string k) + " * " + x + "^" + (string (degree - (2 * k))))
    let k = (T "x" + " + " + T "y" + " + " + T "z")
    let is = mkImplicit (T "x" + " + " + T "y" + " + " + T "z")
    let s = mkShape is  (mkMaterial (fromColor Color.Gold) 0.0)
    let camera = mkCamera (mkPoint 4.0 4.0 4.0) (mkPoint 0.0 -0.5 0.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 500 500 in
    let scene = mkScene [s] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "chmutov" + (string degree) + ".png"))
  
  let render toScreen =
    heart toScreen
//    sphere1 1.0 toScreen;
//    sphere2 1.0 toScreen;
//    planeX toScreen;
//    planeY toScreen;
//    planeZ toScreen;
//    torus 1.5 0.5 toScreen;
//    torus2 1.5 0.5 toScreen;
//    testShape toScreen;
    chmutov 2 toScreen;
//    chmutov 4 toScreen;
//    chmutov 6 toScreen
