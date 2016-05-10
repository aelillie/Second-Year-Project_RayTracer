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
        let camera = mkCamera (mkPoint 0.0 1.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let earthTexture = Texture.mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/earth.jpg"
        let earth = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 earthTexture) 
                      (mergeTransformations [rotateX (Math.PI/4.0);rotateY (System.Math.PI*1.0);translate 0.5 0.5 0.5])
        let plane = mkPlane (mkMatTexture (mkMaterial (fromColor(Color.Gray)) 1.0))

        

        (*******Scene******)
        let scene = mkScene [earth;plane] [light;light1] ambientLight camera 3 in
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
