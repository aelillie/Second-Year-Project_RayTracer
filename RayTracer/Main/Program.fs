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
open PlyParse

[<STAThreadAttribute>]
[<EntryPoint>]
let main argv =

    let folder = "shapes"
    let pi = System.Math.PI

    let render toScreen =
        (******Helper******)
        let mkColourTexture c r = mkMatTexture (mkMaterial (fromColor c) r)

        (*******Light******)
        let light = mkLight (mkPoint 1.0 0.0 7.0) (fromColor Color.White) 1.0
        let light1 = mkLight (mkPoint 0.0 4.0 0.0) (fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 10.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 1000 1000 in
        (*******Shapes******)
        let s = System.Diagnostics.Stopwatch.StartNew()
        let ply = mkPLY "../../../ply/apple.ply" true

        s.Stop() ; printf "Ply parsed in %f seconds\n" s.Elapsed.TotalSeconds
        //let tex = (mkTextureFromFile (fun x y -> (y,x)) "../../../textures/bunny.png")
        let mat c = mkMatTexture (mkMaterial (fromColor c) 0.0)
        let ant1 = transform (mkShape ply (mat Color.Gold)) (mergeTransformations [scale 30.0 30.0 30.0;translate 2.0 0.0 0.0])
        let ant2 = transform (mkShape ply (mat Color.Gold)) (mergeTransformations [scale 30.0 30.0 30.0;translate 4.0 0.0 0.0])
        let ant3 = transform (mkShape ply (mat Color.Gold)) (mergeTransformations [scale 30.0 30.0 30.0;translate -2.0 0.0 0.0])
        let ant4 = transform (mkShape ply (mat Color.Gold)) (mergeTransformations [scale 30.0 30.0 30.0;translate -4.0 0.0 0.0])
        let ant5 = transform (mkShape ply (mat Color.Gold)) (mergeTransformations [scale 30.0 30.0 30.0])
        //let plane = transform (mkPlane (mat Color.LightGray)) (rotateX (pi/2.0)) 

        (*******Scene******)

        let scene = mkScene [ant1;ant2;ant3;ant4;ant5] [light] ambientLight camera 3
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
