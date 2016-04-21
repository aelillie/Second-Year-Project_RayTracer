module Shape
open Point
open Material 
open Vector
open Transformation
open Ray

type Shape =
  | S of Point * float * Material
  | TShape of Shape * Transformation
  | PL of Material * Point * Vector
  | D of Point * float * Material
  | B of Shape list
  | HC of Point * float * float * Material
  | T of Point * Point * Point * Material
  | SC of Shape * Shape * Shape
  | Rec of Point * float * float * Material


val transform : Shape -> Transformation -> Shape

val hit : Ray -> Shape ->( float * Vector * Material ) option

/// Construct a sphere.
/// texture coordinates: [0,1] X [0,1]
val mkSphere : center : Point -> radius : float -> Material -> Shape
/// Construct a rectangle.
/// texture coordinates: [0,1] X [0,1]
val mkRectangle : corner : Point -> width : float -> height : float -> Material -> Shape
/// Constructe a triangle.
val mkTriangle : a:Point -> b:Point -> c:Point -> Material -> Shape
/// Construct a plane with the equation z = 0,
/// i.e. the x-y plane
/// texture coordinates: R X R
val mkPlane : Material -> Shape
/// Construct an implicit surface.
/// texture coordinates: {(0,0)}, i.e. has only a single material
//val mkImplicit : string -> baseShape
/// Load a triangle mesh from a PLY file.
/// texture coordinates: [0,1] X [0,1]

/// construct a hollow cylinder (i.e. open on both ends)
/// texture coordinates: [0,1] X [0,1]
val mkHollowCylinder : center : Point -> radius : float -> height : float -> Material -> Shape
/// construct a solid cylinder (i.e. closed on either end by a disk)
/// texture space: hollow cylinder part is textured like mkHollowCylinder;
///                top and bottom disk are textured like mkDisk
val mkSolidCylinder : center : Point -> radius : float -> height : float -> 
                    cylinder: Material -> bottom : Material -> top : Material -> Shape
/// construct a disk at point p in the plane parallel
/// to the x-y plane
/// texture coordinates: [0,1] X [0,1]
val mkDisc : p : Point -> radius : float -> Material -> Shape
/// construct an axis-aligned box with lower corner low
/// and higher corner high
/// textures: the six faces of the box are textured like mkRectangle
val mkBox : low : Point -> high : Point -> front : Material -> back : Material ->
            top : Material -> bottom : Material -> left : Material -> right : Material  -> Shape