module TracerTest
open Transformation
open Camera
open Shape
open Light
open Tracer
open Ray
open Point
open Vector
open Texture
open System
open System.Drawing

 
  let doTest() =
  (* Input the path (local or absolute) where you want your files to be stored *)
      let path_to_files = ""

      let doRender scene toFile =
        match toFile with
        | Some filename -> Scene.renderToFile scene filename
        | None -> Scene.renderToScreen scene

      //let toScreen = false 
      let renderSphere toScreen =
        let light = mkLight (mkPoint 0.0 3.0 5.0) (Colour.fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1
        let camera = mkCamera (mkPoint 0.0 5.0 8.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
    //    let plane2 = mkPlane (Texture.testTexture)
        let sphere1 = mkSphere (5.0) (Texture.loadTexture "C:\Users\Amalie\Documents\sobillede.jpg")
        let plane = mkPlane (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Black) 0.2) (Material.mkMaterial (Colour.fromColor Color.White) 0.2) 0.0)
        let sphere = mkSphere (3.0) (Texture.stribes(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0)
        let triangle = mkTriangle (Point.mkPoint 0.0 0.0 0.0) (Point.mkPoint 2.0 0.0 0.0) (Point.mkPoint 1.0 2.0 0.0) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0)
      //  let box = mkBox (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 0.1) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 1.0)
        let HC = mkHollowCylinder 2.0 3.0 (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 0.25)
        let disc = mkDisc 5.0 (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 0.1)
        let solid = mkSolidCylinder 1.0 2.0 (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 0.1) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 0.1) (Texture.checkerBoard(Material.mkMaterial (Colour.fromColor Color.Red) 0.0) (Material.mkMaterial (Colour.fromColor Color.Green) 0.0) 0.1)
        let scene = Scene.mkScene [plane;sphere] [light] ambientLight camera 2

        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("transform.png"))

      let renderInsideSphere toScreen =
        let light = mkLight (mkPoint 0.0 0.0 0.0) (Colour.fromColor Color.White) 3.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let camera = mkCamera (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
     //   let sphere = mkSphere 2.0 (Material.mkMaterial (Colour.fromColor Color.Red) 0.0) in
 //     let triangle = mkTriangle (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) (Material.mkMaterial (Colour.fromColor Color.Red) 0.0) in
        let scene = Scene.mkScene [] [light] ambientLight camera 0 in
        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("renderInsideSphere.png"))  
      renderSphere true
      renderInsideSphere false