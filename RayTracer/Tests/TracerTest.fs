module TracerTest

open Camera
open Shape
open Light
open Tracer
open Ray
open Point
open Vector
open System
open System.Drawing

 
  let doTest() =
  (* Input the path (local or absolute) where you want your files to be stored *)
      let path_to_files = ""

      let doRender scene toFile =
        match toFile with
        | Some filename -> Scene.renderToFile scene filename
        | None -> ()
      let toScreen = false 
      let renderSphere toScreen =
        let light = mkLight (mkPoint 0.0 3.0 0.0) (Colour.fromColor Color.White) 1.0 in
        //let light2 = mkLight (mkPoint 0.0 0.0 4.0) (Colour.fromColor Color.White) 0.7 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        let plane = mkPlane(mkPoint 0.0 -10.0 0.0) (mkVector 0.0 -1.0 -0.5) (Material.mkMaterial (Colour.fromColor Color.Red) 0.0)
        let sphere2 = mkSphere (mkPoint 2.0 2.0 -1.2) 1.0 (Material.mkMaterial (Colour.fromColor Color.Green) 0.5) in
        let sphere = mkSphere (mkPoint -1.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.1) in
        let sphere3 = mkSphere (mkPoint 0.0 1.0 -2.5) 1.0 (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.3) in
        let sphere4 = mkSphere (mkPoint 3.0 0.0 1.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Orange) 0.3) in
        let triangle = mkTriangle(mkPoint 2.0 1.0 -3.0) (mkPoint 3.0 1.0 -2.0) (mkPoint 2.5 0.0 -1.0) (Material.mkMaterial (Colour.fromColor Color.Blue) 0.2) in
        let scene = Scene.mkScene [sphere3;sphere4;sphere2;sphere;plane;triangle] [light] ambientLight camera 2 in
        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("renderSphere.png"))
      let renderInsideSphere toScreen =
        let light = mkLight (mkPoint 0.0 0.0 0.0) (Colour.fromColor Color.White) 3.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let camera = mkCamera (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 2.0 (Material.mkMaterial (Colour.fromColor Color.Red) 0.0) in
 //     let triangle = mkTriangle (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) (Material.mkMaterial (Colour.fromColor Color.Red) 0.0) in
        let scene = Scene.mkScene [sphere] [light] ambientLight camera 0 in
        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("renderInsideSphere.png"))  
      renderSphere false
      renderInsideSphere false