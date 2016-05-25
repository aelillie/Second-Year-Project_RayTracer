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
        let light = mkLight (mkPoint 1.0 1.0 10.0) (fromColor Color.White) 1.0
        let light1 = mkLight (mkPoint 0.0 4.0 0.0) (fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 10.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let s = System.Diagnostics.Stopwatch.StartNew()
        let ply = mkPLY "../../../ply/bunny_textured.ply" true
        s.Stop() ; printf "Ply parsed in %f seconds\n" s.Elapsed.TotalSeconds
        let tex = (mkTextureFromFile (fun x y -> (y,x)) "../../../textures/bunny.png")
        let mat c = mkMatTexture (mkMaterial (fromColor c) 0.2)
        let bunny = transform (mkShape ply tex) (mergeTransformations [scale 6.0 6.0 6.0;rotateY (Math.PI/4.0);rotateX (Math.PI/10.0)])
        let p = transform (mkPlane (mat Color.Blue)) (mergeTransformations [rotateX (Math.PI/2.0);translate 0.0 -3.5 2.0])

        (*******Scene******)

        let scene = mkScene [p;bunny] [light] ambientLight camera 3
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
