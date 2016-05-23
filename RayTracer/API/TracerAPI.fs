namespace Tracer

open Shapes

module API = 
  type dummy = unit

  type vector = Vector.Vector
  type point = Point.Point
  type colour = Colour.Colour
  type material = Material.Material
  type shape = Shapes.Shape.Shape
  type baseShape = Shape.BaseShape
  type texture = Texture.Texture
  type camera = Camera.Camera
  type scene = Scene.Scene
  type light = Light.Light
  type ambientLight = Light.AmbientLight
  type transformation = Transformation.Transformation

  let mkVector (x : float) (y : float) (z : float) : vector = Vector.mkVector x y z
  let mkPoint (x : float) (y : float) (z : float) : point = Point.mkPoint x y z
  let fromColor (c : System.Drawing.Color) : colour = Colour.fromColor c
  let mkColour (r : float) (g : float) (b : float) : colour = Colour.mkColour r g b

  let mkMaterial (c : colour) (r : float) : material = Material.mkMaterial c r
  let mkTexture (f : float -> float -> material) : texture = Texture.mkTexture f
  let mkMatTexture (m : material) : texture = Texture.mkMatTexture m

  let mkShape (b : baseShape) (t : texture) : shape = Shape.mkShape b t
  let mkSphere (p : point) (r : float) (m : texture) : shape = Shape.mkSphere p r m
  let mkRectangle (corner : point) (width : float) (height : float) (t : texture) : shape
    = Shape.mkRectangle corner width height t
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = Shape.mkTriangle a b c m
  let mkPlane (t : texture) : shape = Shape.mkPlane t
  let mkImplicit (s : string) : baseShape = failwith "mkImplicit not implemented"
  let mkPLY (filename : string) (smooth : bool) : baseShape = Shape.mkPLY filename smooth

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape 
    = Shape.mkHollowCylinder c r h t 
  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape
      = Shape.mkSolidCylinder c r h t top bottom
  let mkDisc (c : point) (r : float) (t : texture) : shape = Shape.mkDisc c r t
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape
      = Shape.mkBox low high front back top bottom left right
 

  let group (s1 : shape) (s2 : shape) : shape = Shape.group s1 s2
  let union (s1 : shape) (s2 : shape) : shape = Shape.union s1 s2
  let intersection (s1 : shape) (s2 : shape) : shape = Shape.intersection s1 s2
  let subtraction (s1 : shape) (s2 : shape) : shape = Shape.subtraction s1 s2

  let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) : camera 
    = Camera.mkCamera pos look up zoom width height pwidth pheight
  let mkLight (p : point) (c : colour) (i : float) : light = Light.mkLight p c i
  let mkAmbientLight (c : colour) (i : float) : ambientLight = Light.mkAmbientLight c i

  let mkScene (s : shape list) (l : light list) (a : ambientLight) (c : camera) (m : int) : scene 
    = Scene.mkScene s l a c m 
  let renderToScreen (sc : scene) : unit = Scene.renderToScreen sc
  let renderToFile (sc : scene) (path : string) : unit = Scene.renderToFile sc path

  let translate (x : float) (y : float) (z : float) : transformation = Transformation.translate x y z
  let rotateX (angle : float) : transformation = Transformation.rotateX angle
  let rotateY (angle : float) : transformation = Transformation.rotateY angle
  let rotateZ (angle : float) : transformation = Transformation.rotateZ angle
  let sheareXY (distance : float) : transformation = Transformation.sheareXY distance
  let sheareXZ (distance : float) : transformation = Transformation.sheareXZ distance
  let sheareYX (distance : float) : transformation = Transformation.sheareYX distance
  let sheareYZ (distance : float) : transformation = Transformation.sheareYZ distance
  let sheareZX (distance : float) : transformation = Transformation.sheareZX distance
  let sheareZY (distance : float) : transformation = Transformation.sheareZY distance
  let scale (x : float) (y : float) (z : float) : transformation = Transformation.scale x y z
  let mirrorX : transformation = Transformation.mirrorX
  let mirrorY : transformation = Transformation.mirrorY
  let mirrorZ : transformation = Transformation.mirrorZ
  let mergeTransformations (ts : transformation list) : transformation = Transformation.mergeTransformations ts
  let transform (sh : shape) (tr : transformation) : shape = Shapes.TransformedShape.transform sh tr