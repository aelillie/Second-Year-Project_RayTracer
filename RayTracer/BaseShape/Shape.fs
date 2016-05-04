namespace Shapes
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

module Shape = 
    type Shape = Shapes.BasicShape.Shape

    ///Translate a shape to some point
    let moveShape p s = let (x,y,z) = Point.getCoord p in transform s (translate x y z)

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
        = new Rectangle((mkPoint 0.0 0.0 0.0), width, height, t) |> moveShape corner

    //Box
    let mkBox (low : Point) (high : Point) (front : Material) (back : Material) (top : Material) 
                (bottom : Material) (left : Material) (right : Material) 
          = new Box(low, high,front,back,top,bottom,left,right)

    //Sphere
    let mkSphere (p : Point) (r : float) (m : Material) = new Sphere ((mkPoint 0.0 0.0 0.0),r,m) |> moveShape p

    //Cylinders and Discs
    let mkHollowCylinder (c : Point) (r : float) (h : float) (t : Material) = 
        new HollowCylinder((mkPoint 0.0 0.0 0.0),r,h,t) |> moveShape c
    let mkDisc (c : Point) (r : float) (t : Material) = new Disc ((mkPoint 0.0 0.0 0.0),r,t) |> moveShape c
    let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Material) (top : Material) (bottom : Material) 
         = new SolidCylinder((mkPoint 0.0 0.0 0.0),r,h,t,top,bottom) |> moveShape c

    //Triangle
    let mkTriangle a b c mat = new Triangle(a,b,c,mat)
