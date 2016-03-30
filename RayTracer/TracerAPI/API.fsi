namespace Tracer

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

  val mkVector : x:float -> y:float -> z:float -> vector
  val mkPoint : x:float -> y:float -> c:float -> point
  val fromColor : c : System.Drawing.Color -> colour
  val mkColour : r:float -> g:float -> b:float -> colour

  val mkMaterial : colour -> float -> material
  /// Textures are functions that take x-y coordinates and produce a material.
  /// The x-y coordinates range over the texture space specified for the
  /// individual basic shapes (mkSphere, mkPlane etc.).
  val mkTexture : (float -> float -> material) -> texture
  /// Construct a texture with a constant material for each point.
  val mkMatTexture : material -> texture


  /// Construct a textured shape from a base shape.
  /// Basic shapes are textured according to the texture space given.
  val mkShape : baseShape -> texture -> shape

  /// Construct a sphere.
  /// texture space: [0,1] X [0,1]
  val mkSphere : center : point -> radius : float -> texture -> shape
  /// Constructe a triangle.
  /// texture space: [0,1] X [0,1]
  val mkTriangle : a:point -> b:point -> c:point -> texture -> shape
  /// Construct a plane.
  /// texture space: R X R
  val mkPlane : a : point -> normal : vector -> texture -> shape
  /// Construct an implicit surface.
  /// texture space: {(0,0)}
  val mkImplicit : string -> baseShape
  /// Load a triangle mesh from a PLY file.
  /// texture space: [0,1] X [0,1]
  val mkPLY : filename : string -> baseShape
  val union : shape -> shape -> shape
  val intersection : shape -> shape -> shape
  val subtraction : shape -> shape -> shape

 
  val mkCamera : position : point -> lookat : point -> up : vector -> zoom : float -> 
    unitWidth : float -> unitHeight : float -> pixelWidth : int -> pixelHeight : int -> camera

  val mkLight : position : point -> colour : colour -> intensity : float -> light

  val mkAmbientLight : colour : colour -> intensity : float -> ambientLight

  val mkScene : shapes : shape list -> lights : light list -> ambientLight -> camera -> max_reflect : int -> scene
  val renderToScreen : scene -> unit
  val renderToFile : scene -> filename : string -> unit

  val rotateX : angle : float -> transformation
  val rotateY : angle : float -> transformation
  val rotateZ : angle : float -> transformation
  val sheareXY : distance : float -> transformation
  val sheareXZ : distance : float -> transformation
  val sheareYX : distance : float -> transformation
  val sheareYZ : distance : float -> transformation
  val sheareZX : distance : float -> transformation
  val sheareZY : distance : float -> transformation
  val scale : x : float -> y : float -> z : float -> transformation
  val translate : x : float -> y : float -> z : float -> transformation
  val mirrorX : transformation
  val mirrorY : transformation
  val mirrorZ : transformation
  val mergeTransformations : transformation list -> transformation
  val transform : shape -> transformation -> shape
