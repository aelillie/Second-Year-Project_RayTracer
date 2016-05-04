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
open Material
open Colour
open Shapes
open Shapes.Shape
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
        let pi = System.Math.PI
        let plane = transform (mkPlane (Material.mkMaterial (Colour.mkColour 0.35 0.24 0.67) 0.0)) (translate 0.0 -1.0 0.0)
        let blueSphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.2)
        let redSphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Red) 0.2)
        let blueRedSC = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0
                            (Material.mkMaterial (Colour.fromColor Color.Blue) 0.2)
                            (Material.mkMaterial (Colour.fromColor Color.Red) 0.2)
                            (Material.mkMaterial (Colour.fromColor Color.Red) 0.2)
        let greenYellowSC = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0
                                (Material.mkMaterial (Colour.fromColor Color.Green) 0.2)
                                (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.2)
                                (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.2)
        let mkUnitBox l h t = mkBox l h t t t t t t
        let redBox = mkUnitBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) (mkMaterial (fromColor Color.Red) 0.1)
        let blueBox = mkUnitBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) (mkMaterial (fromColor Color.Blue) 0.0)
        let transBox b = transform b (mergeTransformations [rotateY (pi/4.0);rotateX (pi/4.0)])

        let sc2x = let u = union (transform blueRedSC (translate -0.5 0.0 0.0)) 
                                 (transform greenYellowSC (translate 0.5 0.0 0.0))
                   transform u (mergeTransformations [translate 3.0 1.0 0.0; rotateX (pi/4.0);scale 0.7 0.7 0.7]) 
        let blueRedBox = let sub = subtraction 
                                        (transform (transBox redBox) (translate -0.5 0.5 0.0)) 
                                        (transform (transBox blueBox) (translate 0.5 0.5 0.0))
                         transform sub (rotateY -(pi/2.0))
                        

        let blueRedSphere = let brs = intersection (transform blueSphere (translate -0.5 0.0 0.0)) (transform redSphere (translate 0.5 0.0 0.0))
                            transform brs (translate -2.5 1.0 0.0)                    

        let scene = mkScene [plane;blueRedSphere;blueRedBox;sc2x] [light] ambientLight camera 2

        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("transform.png"))

      let renderInsideSphere toScreen =
        let light = mkLight (mkPoint 0.0 0.0 0.0) (Colour.fromColor Color.White) 3.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let camera = mkCamera (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
        let scene = Scene.mkScene [] [light] ambientLight camera 0 in
        if toScreen then
          doRender scene None
        else
          doRender scene (Some ("renderInsideSphere.png"))  
      renderSphere true
      renderInsideSphere false