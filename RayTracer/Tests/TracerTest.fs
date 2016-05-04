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
open Implicit


 
  let doTest() =
  (* Input the path (local or absolute) where you want your files to be stored *)
      let path_to_files = ""

      let doRender scene toFile =
        match toFile with
        | Some filename -> Scene.renderToFile scene filename
        | None -> ()
      let toScreen = false
      let renderSphere toScreen =
        let light = mkLight (mkPoint 0.0 3.0 5.0) (Colour.fromColor Color.White) 1.0
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1
        let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
        let plane = mkPlane(mkPoint 0.0 0.0 0.0) (mkVector 0.0 -3.0 -1.0) (Material.mkMaterial (Colour.mkColour 0.35 0.24 0.67) 0.0)
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.2)
        let sphere1 = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.8)
        let sphere2 = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (Material.mkMaterial (Colour.fromColor Color.Purple) 0.2)
        let sphere3 = mkSphere (mkPoint 0.0 0.0 0.0) 3.0 (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.2)

        let hc = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 (Material.mkMaterial (Colour.fromColor Color.Blue) 0.0)
        let sc = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 
                    (Material.mkMaterial (Colour.fromColor Color.Purple) 0.2)
                    (Material.mkMaterial (Colour.fromColor Color.Orange) 0.2)
                    (Material.mkMaterial (Colour.fromColor Color.Orange) 0.2)
        let box = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) 
                    (Material.mkMaterial (Colour.fromColor Color.Red) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Green) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Blue) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.White) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.0)
                    (Material.mkMaterial (Colour.fromColor Color.Purple) 0.0)
        let tr = mkTriangle (mkPoint 1.0 0.0 0.0) (mkPoint 0.0 1.0 0.0) (mkPoint 0.0 0.0 1.0)
                    (Material.mkMaterial (Colour.fromColor Color.White) 0.0)
        
        let implicitPlane = mkShape (mkImplicit "0*x+-3*y+-1*z+d" ("d",0.0)) (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.2)
        let implicitSphere = mkShape (mkImplicit "x^2+y^2+z^2+-1r^2" ("r",1.0)) (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.8)
        let implicitSphere2 = mkShape (mkImplicit "x^2+y^2+z^2+-1r^2" ("r",1.0)) (Material.mkMaterial (Colour.fromColor Color.Yellow) 0.05)
        let implicitSphere3 = mkShape (mkImplicit "x^2+y^2+z^2+-1r^2" ("r",1.0)) (Material.mkMaterial (Colour.fromColor Color.Green) 0.05)
        let implicitSphere4 = mkShape (mkImplicit "x^2+y^2+z^2+-1r^2" ("r",1.0)) (Material.mkMaterial (Colour.fromColor Color.Blue) 0.2)
        let tsphere = transform sphere1 (mergeTransformations [translate 1.0 0.0 0.0])
        let tsphere1 = transform sphere1 (mergeTransformations [scale 1.5 1.5 1.5;translate -1.0 1.5 -2.0])
        let tsphere2 = transform sphere2 (mergeTransformations [rotateZ System.Math.PI;translate -2.0 1.0 1.0])
        let timplic1 = transform implicitSphere (mergeTransformations [translate -1.0 0.0 0.0])
        let timplic2 = transform implicitSphere2 (mergeTransformations [translate -1.0 0.0 0.0])
        let timplic3 = transform implicitSphere3 (mergeTransformations [translate -1.0 1.0 -1.5])
        let timplic4 = transform implicitSphere4 (mergeTransformations [translate 1.0 0.0 0.0])
        let tsc = transform sc (mergeTransformations [scale 0.7 0.7 0.7;rotateX (System.Math.PI / 4.0);translate -4.0 -2.0 0.0])
        let tbox = transform box (mergeTransformations [scale 0.7 0.7 0.7;rotateY (System.Math.PI / 0.07543532);rotateX (System.Math.PI / 0.07543532)])
        let ttr = transform tr (translate -3.0 2.0 0.0)
       
        let scene = Scene.mkScene [timplic1;tsphere] [light] ambientLight camera 2

        let scene2 = Scene.mkScene [implicitPlane] [light] ambientLight camera 2
    

        if toScreen then
          doRender scene2 None
        else
          doRender scene2 (Some ("transformPlane.png"))

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