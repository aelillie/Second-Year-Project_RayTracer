module Shape
open Point
open Vector
open Ray
open ExprParse
open Material
open Transformation
open PlyParse
open Shapes.AdvancedShape
open Shapes.BasicShape
open Shapes.TransformedShape


type Shape = Shapes.BasicShape.Shape

//Collect a group of shapes as one union
let group s1 s2 = new GroupShape(s1, s2)      
                     
//Union compose two shapes
let union s1 s2  = new UnionShape(s1,s2)
//Keep the difference between two shapes

let intersection s1 s2  = new IntersectionShape(s1,s2)
//Subtract s2 from s1 (s2-s1)
let subtraction s1 s2  = new SubtractionShape(s1,s2)

//Plane
let mkPlane (material : Material) = new Plane(material)

//Rectangle
let mkRectangle (corner : Point) (width : float) (height : float) (t : Material)
    = new Rectangle(corner, width, height, t)

//Box
let mkBox (low : Point) (high : Point) (front : Material) (back : Material) (top : Material) (bottom : Material) (left : Material) (right : Material) 
      = new Box(low,high,front,back,top,bottom,left,right)
let mkBoxCenter front back top bottom left right = 
        mkBox (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 0.0) front back top bottom left right

//Sphere
let mkSphere (p : Point) (r : float) (m : Material) = new Sphere (p,r,m)
let mkSphereCenter (r : float) (m : Material) = mkSphere (mkPoint 0.0 0.0 0.0) r m

//Cylinders and Discs
let mkHollowCylinder (c : Point) (r : float) (h : float) (t : Material) = new HollowCylinder(c,r,h,t)
let mkHollowCylinderCenter r h t = mkHollowCylinder (mkPoint 0.0 0.0 0.0) r h t
let mkDisc (c : Point) (r : float) (t : Material) = new Disc (c,r,t)
let mkDiscCenter r t = mkDisc (mkPoint 0.0 0.0 0.0) r t
let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Material) (top : Material) (bottom : Material) 
     = new SolidCylinder(c,r,h,t,top,bottom)
let mkSolidCylinderCenter r h t top bottom = mkSolidCylinder (mkPoint 0.0 0.0 0.0) r h t top bottom

//Triangle
let mkTriangle a b c mat = new Triangle(a,b,c,mat)
