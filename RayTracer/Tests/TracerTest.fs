module TracerTest
open Transformation

open PlyParse
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
        let plyFile = parsePly "C:\Users\SecondBanana\Documents\GitHubVisualStudio\Ray-Tracer-Project\RayTracer\ant.ply"
        let ant = mkTriangleMesh (mkPoint 0.0 0.0 -40.0) plyFile
        let scene = Scene.mkScene [ant] [light] ambientLight camera 2 in
        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("transform.png"))

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