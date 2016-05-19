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
open Texture
open Material
open Colour

[<STAThreadAttribute>]
[<EntryPoint>]
let main argv =

    let folder = "shapes"
    let render toScreen =
        (*******Light******)
        let light = mkLight (mkPoint 0.0 5.0 5.0) (fromColor Color.White) 1.0 in
        let light2 = mkLight (mkPoint -10.0 30.0 0.0) (fromColor Color.White) 1.0 in
        let light1 = mkLight (mkPoint 10.0 30.0 0.0) (fromColor Color.White) 1.0 in
        let light3 = mkLight (mkPoint 0.0 5.0 25.0) (fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 25.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let tm = mkPLY "C:\\Program Files\\ant.ply" false
        let tm' = mkShape tm (Texture.mkMatTexture ((Material.mkMaterial (Colour.fromColor Color.Green)) 0.0))
        let elapsed = stopWatch.Elapsed.TotalSeconds
        printfn("%f") elapsed
        let tm'' = transform tm' (translate 0.0 0.0 0.0)
        let plane = mkPlane (Texture.mkMatTexture ((Material.mkMaterial (Colour.fromColor Color.Blue)) 0.0))
        let plane' = transform plane (rotateX (System.Math.PI/2.0))
        (*******Scene******)
        let scene = mkScene [tm'] [light;light2;light1;light3] ambientLight camera 0 in
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
