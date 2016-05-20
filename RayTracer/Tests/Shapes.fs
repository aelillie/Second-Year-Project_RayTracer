namespace TestSuite

open Shapes
open Texture
open Material
open Colour
open Shape
open Point
open Vector
open Light
open Scene
open Camera
open TransformedShape
open Transformation
open System
open System.Drawing
open Scene
open Util

module Shapes =

  let private folder = "shapes"

  let renderSphere toScreen =
    let light = mkLight (mkPoint 0.0 0.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0)) in
    let scene = mkScene [sphere] [light] ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "renderSphere.png"))

  let renderHollowCylinder toScreen =
    let light = mkLight (mkPoint 2.0 3.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let camera = mkCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 -0.5) 18.0 4.0 4.0 500 500 in
    let cylinder = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 2.0 1.0 (mkMatTexture (mkMaterial (fromColor Color.Yellow) 0.0)) in
    let scene = mkScene [cylinder] [light] ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "renderHollowCylinder.png"))

  let renderSolidCylinder toScreen =
    let light = mkLight (mkPoint 2.0 3.0 4.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let camera = mkCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 -0.5) 18.0 4.0 4.0 500 500 in
    let cylinder = 
      mkSolidCylinder (mkPoint 0.0 0.0 0.0) 2.0 1.0 (mkMatTexture (mkMaterial (fromColor Color.Yellow) 0.0))
        (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)) (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)) in
    let scene = mkScene [cylinder] [light] ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "renderSolidCylinder.png"))

  let renderInsideSphere toScreen =
    let light = mkLight (mkPoint 0.0 0.0 0.0) (fromColor Color.White) 1.0 in
    let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
    let camera = mkCamera (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500 in
    let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)) in
    let scene = mkScene [sphere] [light] ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "renderInsideSphere.png"))


  let render toScreen =
    renderSphere toScreen;
    renderHollowCylinder toScreen;
    renderSolidCylinder toScreen;
    renderInsideSphere toScreen