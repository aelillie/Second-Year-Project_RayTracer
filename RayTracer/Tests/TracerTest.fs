module TracerTest
open Transformation
open Camera
open Light
open Tracer
open Ray
open Point
open Vector
open System
open System.Drawing
open Scene
open Shape
open Shapes
open TransformedShape

  let doTest() =
  (* Input the path (local or absolute) where you want your files to be stored *)
      let path_to_files = ""

      let doRender scene toFile =
        match toFile with
        | Some filename -> Scene.renderToFile scene filename
        | None -> Scene.renderToScreen scene

      //let toScreen = false 
      let renderSphere toScreen =
        let light = mkLight (mkPoint -1.5 0.0 3.0) (Colour.fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500

        let plane = transform (mkPlane (Material.mkMaterial (Colour.mkColour 0.35 0.24 0.67) 0.0)) (translate 0.0 -1.0 0.0)
        let blueSphere = mkSphereCenter 1.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.2)
        let redSphere = mkSphereCenter 1.0 (Material.mkMaterial (Colour.fromColor Color.Red) 0.2)

        let blueRedSphere = union (transform blueSphere (translate -0.5 0.0 0.0)) (transform redSphere (translate 0.5 0.0 0.0))                    

        let scene = mkScene [plane;blueRedSphere] [light] ambientLight camera 2

        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("transform.png"))

      let renderInsideSphere toScreen =
        let light = mkLight (mkPoint 0.0 0.0 0.0) (Colour.fromColor Color.White) 3.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let camera = mkCamera (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
       // let sphere = mkSphereCenter 2.0 (Material.mkMaterial (Colour.fromColor Color.Red) 0.0) in
 //     let triangle = mkTriangle (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) (Material.mkMaterial (Colour.fromColor Color.Red) 0.0) in
        let scene = Scene.mkScene [] [light] ambientLight camera 0 in
        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("renderInsideSphere.png"))  
      renderSphere true
      renderInsideSphere false