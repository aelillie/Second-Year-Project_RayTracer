//Entry point for running the program

open Utilities
open Tracer.API
open Texture
open System
open System.Drawing

[<STAThreadAttribute>]
[<EntryPoint>]
let main argv =

    let folder = "shapes"
    let pi = System.Math.PI

    let render toScreen =
        (*******Light******)
        let light = mkLight (mkPoint 2.0 4.0 8.0) (fromColor Color.White) 1.0
        let light1 = mkLight (mkPoint -2.0 4.0 8.0) (fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 10.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 1000 1000 in
        (*******Shapes******)
        let s = System.Diagnostics.Stopwatch.StartNew()
        let ply = mkPLY "../../../ply/bunny_textured.ply" true
        let imp = mkImplicit ("(((x^2 + y^2)_2 + -1.5)^2 + z^2)_2 + -0.5")
        s.Stop() ; printf "Ply parsed in %f seconds\n" s.Elapsed.TotalSeconds
        let tex = (mkTextureFromFile (fun x y -> (x,y)) "../../../textures/bunny.png")
        let mat c r = mkMatTexture (mkMaterial (fromColor c) r)
//        let torus = transform (mkShape imp (mat Color.Yellow 0.0)) (mergeTransformations [rotateX (pi/2.0);scale 0.5 0.5 0.5; translate -1.6 5.5 0.0])
        let bunny = transform (mkShape ply tex) (mergeTransformations [scale 5.0 5.0 5.0;rotateY (Math.PI/4.0);rotateX (Math.PI/14.0);translate 0.0 0.5 0.0])
        let sphere = mkSphere (mkPoint 0.0 1.0 0.0) 1.0 (mat Color.Red 0.0)
        let sphere' = transform sphere (mergeTransformations [translate 1.0 3.0 -2.0])
//        let p = transform (mkPlane (mat Color.Blue 0.2)) (mergeTransformations [rotateX (Math.PI/2.0);translate 0.0 -2.5 2.0])

//        let sc = mkSolidCylinder (mkPoint 0.0 1.0 0.0) 1.0 3.0 (mat Color.Green 0.0) (mat Color.Yellow 0.0) (mat Color.Yellow 0.0)
//        let sc' = transform sc (mergeTransformations [translate -0.5 0.0 0.0])
//        let s = intersection sphere' sc

        (*******Scene******)

        let scene = mkScene [sphere'] [light] ambientLight camera 3
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
