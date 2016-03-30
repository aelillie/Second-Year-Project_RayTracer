﻿namespace Tracer

module API = 
  type dummy = unit

  type vector = dummy
  type point = dummy
  type colour = dummy
  type material = dummy
  type shape = dummy
  type baseShape = dummy
  type texture = dummy
  type camera = dummy
  type scene = dummy
  type light = dummy
  type ambientLight = dummy
  type transformation = dummy

  let mkVector (x : float) (y : float) (z : float) : vector = failwith "mkVector not implemented"
  let mkPoint (x : float) (y : float) (z : float) : point = failwith "mkPoint not implemented"
  let fromColor (c : System.Drawing.Color) : colour = failwith "fromColor not implemented"
  let mkColour (r : float) (g : float) (b : float) : colour = failwith "mkColour not implemented"

  let mkMaterial (c : colour) (r : float) : material = failwith "mkMaterial not implemented"
  let mkTexture (f : float -> float -> material) : texture = failwith "mkTexture not implemented"
  let mkMatTexture (m : material) : texture = failwith "mkMatTexture not implemented"

  let mkShape (b : baseShape) (t : texture) : shape = failwith "mkShape not implemented"
  let mkSphere (p : point) (r : float) (m : material) : shape = failwith "mkSphere not implemented"
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = failwith "mkTriangle not implemented"
  let mkPlane (p : point) (n : vector) (m : material) : shape = failwith "mkPlane not implemented"
  let mkImplicit (s : string) : baseShape = failwith "mkImplicit not implemented"
  let mkPLY (filename : string) : baseShape = failwith "mkPLY not implemented"

  let union (s1 : shape) (s2 : shape) : shape = failwith "union not implemented"
  let intersection (s1 : shape) (s2 : shape) : shape = failwith "intersection not implemented"
  let subtraction (s1 : shape) (s2 : shape) : shape = failwith "subtraction not implemented"

  let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) : camera = failwith "mkCamera not implemented"
  let mkLight (p : point) (c : colour) (i : float) : light = failwith "mkLight not implemented"
  let mkAmbientLight (c : colour) (i : float) : ambientLight = failwith "mkAmbientLight not implemented"

  let mkScene (s : shape list) (l : light list) (a : ambientLight) (c : camera) (m : int) : scene = failwith "mkScene not implemented"
  let renderToScreen (sc : scene) : unit = failwith "renderToScreen not implemented"
  let renderToFile (sc : scene) (path : string) : unit = failwith "renderToFile not implemented"

  let translate (x : float) (y : float) (z : float) : transformation = failwith "translate not implemented"
  let rotateX (angle : float) : transformation = failwith "rotateX not implemented"
  let rotateY (angle : float) : transformation = failwith "rotateY not implemented"
  let rotateZ (angle : float) : transformation = failwith "rotateZ not implemented"
  let sheareXY (distance : float) : transformation = failwith "sheareXY not implemented"
  let sheareXZ (distance : float) : transformation = failwith "sheareXZ not implemented"
  let sheareYX (distance : float) : transformation = failwith "sheareYX not implemented"
  let sheareYZ (distance : float) : transformation = failwith "sheareYZ not implemented"
  let sheareZX (distance : float) : transformation = failwith "sheareZX not implemented"
  let sheareZY (distance : float) : transformation = failwith "sheareZY not implemented"
  let scale (x : float) (y : float) (z : float) : transformation = failwith "scale not implemented"
  let mirrorX : transformation = failwith "mirrorX not implemented"
  let mirrorY : transformation = failwith "mirrorX not implemented"
  let mirrorZ : transformation = failwith "mirrorX not implemented"
  let mergeTransformations (ts : transformation list) : transformation = failwith "mergeTransformation not implemented"
  let transform (sh : shape) (tr : transformation) : shape = failwith "transform not implemented"
