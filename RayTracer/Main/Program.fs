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
        (*******Light******)
        let light = mkLight (mkPoint 0.0 3.0 5.0) (fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMaterial(Colour.fromColor Color.Blue) 0.0)) in
        let plane = mkPlane (Texture.loadTexture "C:\Users\Amalie\Documents\checkerboard2.jpg")
        let unitBox p1 p2 t = mkBox p1 p2 t t t t t t
        let box = unitBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) (Texture.loadTexture "C:\Users\Amalie\Documents\checkerboard2.jpg")
        let disc = mkDisc (mkPoint 0.0 0.0 0.0) 3.4 (Texture.loadTexture "C:\Users\Amalie\Documents\checkerboard2.jpg")
        let HC = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 2.0 4.0 (Texture.loadTexture "C:\Users\Amalie\Documents\sobillede.jpg")
        let SC = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 2.0 4.0 (Texture.loadTexture "C:\Users\Amalie\Documents\sobillede.jpg")(Texture.loadTexture "C:\Users\Amalie\Documents\sobillede.jpg")(Texture.loadTexture "C:\Users\Amalie\Documents\sobillede.jpg")
        let SC' = transform SC (rotateY (System.Math.PI))

        (*******Scene******)
        let scene = mkScene [SC'] [light] ambientLight camera 0 in
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
