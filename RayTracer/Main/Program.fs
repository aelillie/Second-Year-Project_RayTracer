﻿//Entry point for running the program
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
        (*******Light******)
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let light = mkLight (mkPoint 1.0 1.0 4.0) (fromColor Color.White) 1.0
        let light1 = mkLight (mkPoint -2.0 1.0 4.0) (fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        (*******Camera******)
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        (*******Shapes******)
        let ply = mkPLY "../../../ply/bunny_textured.ply" false
        let texture = mkTextureFromFile (fun x y -> (y, x)) "../../../textures/bunny.png"
        let t s = transform s (mergeTransformations [scale 0.5 0.5 0.5])
//        let ant = mkShape ply (mkMatTexture (mkMaterial (fromColor Color.Gray) 0.0)) |> t
        let bunny = mkShape ply texture
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))            
        sw.Stop()

        printf "%f" sw.Elapsed.TotalSeconds

        (*******Scene******)

        let scene = mkScene [bunny] [light] ambientLight camera 3
        if toScreen then
          Util.render scene None
        else
          Util.render scene (Some (folder, "image.png"))

    
    render true

    0 // return an integer exit code
