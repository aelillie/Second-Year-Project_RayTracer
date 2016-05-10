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

open System.Drawing

[<EntryPoint>]
let main argv =

    let folder = "shapes"
     
    let render toScreen =
        (******Helper******)
        let mkColourTexture c r = mkMatTexture (mkMaterial (fromColor c) r)

        (*******Light******)
        let light = mkLight (mkPoint 0.0 0.0 4.0) (fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let SC = mkSolidCylinder (mkPoint 0.0 0.5 0.0) 1.0 2.0 (mkColourTexture Color.Red 0.0) (mkColourTexture Color.Red 0.0) (mkColourTexture Color.Red 0.0)
        let Box = mkBox (mkPoint -1.0 0.0 0.0) (mkPoint 1.0 1.0 1.0) (mkColourTexture Color.Yellow 0.0) (mkColourTexture Color.Yellow 0.0) (mkColourTexture Color.Yellow 0.0) (mkColourTexture Color.Yellow 0.0) (mkColourTexture Color.Yellow 0.0) (mkColourTexture Color.Yellow 0.0) 
        let Sphere = mkSphere (mkPoint 0.0 0.5 0.0) 1.0 (mkColourTexture Color.Blue 0.0)
        let Sphere2 = mkSphere (mkPoint 0.0 0.5 2.0) 1.0 (mkColourTexture Color.Blue 0.0)
        let u = union Box Sphere
        let SC' = subtraction SC u
        let SC' = transform SC' (rotateY (System.Math.PI/4.0))
        (*******Scene******)
        let scene = mkScene [SC'] [light] ambientLight camera 0 in
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
