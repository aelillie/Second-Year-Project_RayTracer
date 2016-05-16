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
open Shapes.TransformedShape
open Shapes
open Texture
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
        let light = mkLight (mkPoint 2.0 1.0 4.0) (fromColor Color.White) 1.0
        let light1 = mkLight (mkPoint -2.0 1.0 4.0) (fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 1.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 1000 1000 in
        (*******Shapes******)
        let sc = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)) (mkMatTexture (mkMaterial (fromColor Color.Beige) 0.0)) (mkMatTexture (mkMaterial (fromColor Color.Beige) 0.0))
        let sc' = transform sc (rotateX (System.Math.PI / 4.0))
        

        (*******Scene******)
        let scene = mkScene [sc'] [light] ambientLight camera 3
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
