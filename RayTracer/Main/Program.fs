//Entry point for running the program
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
open ImplicitShape

open System.Drawing

[<EntryPoint>]
let main argv =

    let folder = "shapes"

    let render toScreen =
        (*******Light******)
        let light = mkLight (mkPoint 0.0 0.0 4.0) (fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMaterial (fromColor Color.Blue) 0.0) in

        let implicitSphere = mkShape (mkImplicit "x^2+y^2+z^2+-1r^2") (mkMaterial (fromColor Color.Yellow) 0.2) in
        let tSphere = transform implicitSphere (translate 0.0 1.0 0.0) 
        let implicitPlane = mkShape (mkImplicit "0*x+-3y+-1z+d") (mkMaterial (fromColor Color.Blue) 0.1) in

        (*******Scene******)
        let scene = mkScene [tSphere;implicitPlane] [light] ambientLight camera 0 in
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
