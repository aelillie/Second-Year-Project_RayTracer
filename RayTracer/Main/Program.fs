﻿//Entry point for running the program
open Scene
open Light
open Point
open Camera
open Colour
open Shapes
open Utilities
open Vector
open Material
open Shape
open Implicit
open TransformedShape
open Transformation
open System
open System.Drawing

[<STAThreadAttribute>]
[<EntryPoint>]
let main argv =

    let folder = "shapes"

    let render toScreen =


        
        (*******Light******)
        let light = mkLight (mkPoint 0.0 0.0 4.0) (fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.8 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 2.0 8.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500 in
        (*******Shapes******)
//        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMaterial (fromColor Color.Blue) 0.0) in
//
//        let implicitSphere (r:float) = mkShape (mkImplicit ("x^2+y^2+z^2+-1*" + (string (r)))) (mkMaterial (fromColor Color.Red) 0.0) in
//        let implicitSphere2 (r:float) = mkShape (mkImplicit ("x^2+y^2+z^2+-1*" + (string (r)))) (mkMaterial (fromColor Color.Blue) 0.0) in
//
//
//        let tSphere = transform (implicitSphere 1.0) (translate -1.0 1.0 0.0) 
//        let t2 = transform (implicitSphere2 1.0) (translate 1.5 0.0 0.0)
//        
        let implicitPlane = mkShape (mkImplicit  "(x + -2)^2(x+2)^2 + (y + -2)^2(y+2)^2 + (z + -2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z + -10(x^2 + y^2 + z^2) + 22") (mkMaterial (fromColor Color.Blue) 0.0) in //(x + -2)^2(x+2)^2 + (y + -2)^2(y+2)^2 + (z + -2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z + -10(x^2 + y^2 + z^2) + 22
//        let tPlane = transform (implicitPlane) (rotateX (System.Math.PI/2.0)) 

        (*******Scene******)
        let scene = mkScene [implicitPlane ] [light] ambientLight camera 0 in
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
