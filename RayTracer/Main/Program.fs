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
        let earthTexture = Texture.mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/earth.jpg" 0.1
        let marsTexture = Texture.mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/mars.jpg" 0.1
        let mercuryTexture = Texture.mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/mercury.jpg" 0.1
        let jupiterTexture = Texture.mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/jupiter.jpg" 0.1
        let sunTexture = Texture.mkTextureFromFile (fun x y -> (x,1.0-y)) "../../../textures/sun.jpg" 0.1
        let earth = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 earthTexture) 
                      (mergeTransformations [scale 0.5 0.5 0.5;rotateX (Math.PI/4.0);rotateY (System.Math.PI*1.0);translate -4.0 1.5 2.0])
        let mars = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 marsTexture) 
                      (mergeTransformations [scale 0.4 0.4 0.4;translate 4.0 -1.5 -1.0])
        let mercury = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 mercuryTexture) 
                       (mergeTransformations [scale 0.3 0.3 0.3;translate -5.0 3.0 0.0])
        let jupiter = transform (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 jupiterTexture) 
                       (mergeTransformations [scale 0.7 0.7 0.7;translate 5.0 -2.0 0.0])
        let sun = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 sunTexture
        let unitBox low high t = mkBox low high t t t t t t
        let plane = mkPlane (mkMatTexture (mkMaterial (fromColor(Color.Gray)) 0.5))

        

        (*******Scene******)
        let scene = mkScene [earth;mars;mercury;jupiter;sun] [light] ambientLight camera 3
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
