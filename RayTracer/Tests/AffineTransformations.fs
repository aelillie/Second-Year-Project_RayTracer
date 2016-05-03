namespace TestSuite
open System
open System.Drawing
open Light
open Transformation
open Point
open Colour
open Scene
open Camera
open Vector
open Material
open Util
open Shapes

module AffineTransformationsTest = ()
 (* let folder = "affineTransformations"
  let degrees_to_radians (d : int) = (float d) * Math.PI / 180.0

  let mkCube t = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) t t t t t t

  let ocube = mkCube (mkMaterial (fromColor Color.Gold) 0.0)
  let pcube = transform ocube (translate 1.0 1.0 1.0)
  let light = mkLight (mkPoint 4.0 2.0 4.0) (fromColor Color.White) 1.0
  let light2 = mkLight (mkPoint -4.0 2.0 4.0) (fromColor Color.White) 1.0
  let lights = [light; light2]
  let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
  let camera = mkCamera (mkPoint 20.0 20.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 18.0 4.0 4.0 500 500 in
  let cx = transform (mkHollowCylinder (mkPoint 0.0 0.0 0.0) 0.1 100.0 (mkMaterial (fromColor Color.Blue) 0.0)) (rotateZ (Math.PI / 2.0))
  let cy = transform (mkHollowCylinder (mkPoint 0.0 0.0 0.0) 0.1 100.0 (mkMaterial (fromColor Color.GreenYellow) 0.0)) (rotateX (Math.PI / 2.0))
  let cz = mkHollowCylinder (mkPoint 0.0 0.0 0.0) 0.1 100.0 (mkMaterial (fromColor Color.Red) 0.0)
  let axes = group cx (group cy cz)

  let obaseImage toScreen =
    let scene = mkScene [ocube;axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "originBase.png"))

  let pbaseImage toScreen =
    let scene = mkScene [pcube;axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "positiveBase.png"))

  let testRotateX angle toScreen =
    let s = transform pcube (rotateX (degrees_to_radians angle)) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "rotateX_" + (string angle) + ".png"))

  let testRotateY angle toScreen =
    let s = transform pcube (rotateY (degrees_to_radians angle)) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "rotateY_" + (string angle) + ".png"))

  let testRotateZ angle toScreen =
    let s = transform pcube (rotateZ (degrees_to_radians angle)) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "rotateZ_" + (string angle) + ".png"))

  let testMirrorX toScreen =
    let s = transform pcube mirrorX in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "mirrorX.png"))

  let testMirrorY toScreen =
    let s = transform pcube mirrorY in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "mirrorY.png"))

  let testMirrorZ toScreen =
    let s = transform pcube mirrorZ in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "mirrorZ.png"))

  let testScaleX x toScreen =
    let s = transform ocube (scale x 1.0 1.0) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "scaleX.png"))

  let testScaleY y toScreen =
    let s = transform ocube (scale 1.0 y 1.0) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "scaleY.png"))

  let testScaleZ z toScreen =
    let s = transform ocube (scale 1.0 1.0 z) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "scaleZ.png"))

  let testSheareXY d toScreen =
    let s = transform ocube (sheareXY d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sheareXY.png"))

  let testSheareXZ d toScreen =
    let s = transform ocube (sheareXZ d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sheareXZ.png"))

  let testSheareYX d toScreen =
    let s = transform ocube (sheareYX d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sheareYX.png"))

  let testSheareYZ d toScreen =
    let s = transform ocube (sheareYZ d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sheareYZ.png"))

  let testSheareZX d toScreen =
    let s = transform ocube (sheareZX d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sheareZX.png"))

  let testSheareZY d toScreen =
    let s = transform ocube (sheareZY d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "sheareZY.png"))

  let testTranslateX d toScreen =
    let s = transform ocube (translate d 0.0 0.0) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "translateX.png"))

  let testTranslateY d toScreen =
    let s = transform ocube (translate 0.0 d 0.0) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "translateY.png"))

  let testTranslateZ d toScreen =
    let s = transform ocube (translate 0.0 0.0 d) in
    let scene = mkScene [s; axes] lights ambientLight camera 0 in
    if toScreen then
      Util.render scene None
    else
      Util.render scene (Some (folder, "translateZ.png"))

  let render toScreen =
    obaseImage toScreen
    pbaseImage toScreen
    testRotateX 90 toScreen
    testRotateX 180 toScreen
    testRotateX 270 toScreen
    testRotateY 90 toScreen
    testRotateY 180 toScreen
    testRotateY 270 toScreen
    testRotateZ 90 toScreen
    testRotateZ 180 toScreen
    testRotateZ 270 toScreen
    testMirrorX toScreen
    testMirrorY toScreen
    testMirrorZ toScreen
    testScaleX 2.0 toScreen
    testScaleY 2.0 toScreen
    testScaleZ 2.0 toScreen
    testSheareXY 1.0 toScreen
    testSheareXZ 1.0 toScreen
    testSheareYX 1.0 toScreen
    testSheareYZ 1.0 toScreen
    testSheareZX 1.0 toScreen
    testSheareZY 1.0 toScreen
    testTranslateX 1.0 toScreen
    testTranslateY 1.0 toScreen
    testTranslateZ 1.0 toScreen*)
