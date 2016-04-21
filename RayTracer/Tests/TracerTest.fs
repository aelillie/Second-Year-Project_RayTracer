module TracerTest
open Transformation
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
        let light = mkLight (mkPoint -2.0 0.0 5.0) (Colour.fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
        let plane = mkPlane(mkPoint 0.0 -10.0 0.0) (mkVector 0.0 -1.0 -0.5) (Material.mkMaterial (Colour.mkColour 0.35 0.24 0.67) 0.0)
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.0)
        let sphere1 = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.2)
        let sphere2 = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Purple) 0.2)

        let hc = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.0)
        let sc = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 
                    (Material.mkMaterial (Colour.fromColor Color.Orange) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Orange) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Orange) 0.0)
        let box = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) 
                    (Material.mkMaterial (Colour.fromColor Color.Red)    0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Green)  0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Orange)   0.0)
                    (Material.mkMaterial (Colour.fromColor Color.White)  0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Purple) 0.0)
        let tr = mkTriangle (mkPoint 1.0 0.0 0.0) (mkPoint 0.0 1.0 0.0) (mkPoint 0.0 0.0 1.0)
                    (Material.mkMaterial (Colour.fromColor Color.White) 0.0)

        let tsphere = transform sphere (mergeTransformations [scale 1.3 1.3 1.3;translate -0.5 0.0 0.0])
        let tsphere1 = transform sphere1 (translate 0.5 0.0 0.0)
        let usphere = union tsphere tsphere1
        let tusphere = transform usphere (mergeTransformations [rotateZ (System.Math.PI/2.0);sheareXY 1.0])
        let tsphere2 = transform sphere2 (mergeTransformations [rotateZ System.Math.PI;translate -2.0 1.0 1.0])
        let tsc = transform sc (rotateX (System.Math.PI / 2.0))
        let tbox = transform box (mergeTransformations [rotateY (System.Math.PI/4.0);rotateX (System.Math.PI/4.0)])
        let uboxsphere = union tbox tsphere
        let ttr = transform tr (translate -3.0 2.0 0.0)
        let scene = Scene.mkScene [plane;uboxsphere] [light] ambientLight camera 2

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