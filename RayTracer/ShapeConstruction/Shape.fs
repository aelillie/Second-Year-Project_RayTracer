﻿namespace Shapes
open Point
open Vector
open Ray
open ExprParse
open Material
open Transformation
open PlyParse
open Implicit
open Shapes.AdvancedShape
open Shapes.BasicShape
open Shapes.TransformedShape
open Texture
open Shapes.ImplicitShape

module Shape = 
    type Shape = Shapes.BasicShape.Shape
    type simpleExpr = ExprToPoly.simpleExpr
    type BaseShape =
        | PLY of Ply list * bool
        | Bs of poly*simpleExpr
        | Dummy of unit

    ///Translate a shape to some point
    let moveShape p s = let (x,y,z) = Point.getCoord p in transform s (translate x y z)

    //Collect a group of shapes as one union
    let group s1 s2 = new GroupShape(s1, s2) :> Shape  
                     
    //Union compose two shapes
    let union s1 s2  = new UnionShape(s1,s2) :> Shape
    //Keep the difference between two shapes

    let intersection s1 s2  = new IntersectionShape(s1,s2) :> Shape
    //Subtract s2 from s1 (s2-s1)
    let subtraction s1 s2  = new SubtractionShape(s1,s2) :> Shape

    //Plane
    let mkPlane (tex : Texture) = new Plane(tex) :> Shape

    //Rectangle
    let mkRectangle (corner : Point) (width : float) (height : float) (tex : Texture)
        = new Rectangle((mkPoint 0.0 0.0 0.0), width, height, tex) |> moveShape corner

    //Box
    let mkBox (low : Point) (high : Point) (front : Texture) (back : Texture) (top : Texture) 
                (bottom : Texture) (left : Texture) (right : Texture) 
          = new Box(low, high,front,back,top,bottom,left,right) :> Shape

    //Sphere
    let mkSphere (p : Point) (r : float) (tex : Texture) = 
        new Sphere ((mkPoint 0.0 0.0 0.0),r,tex) |> moveShape p 
    let mkSphereCenter r tex = new Sphere (mkPoint 0.0 0.0 0.0,r,tex) 
    //Cylinders and Discs
    let mkHollowCylinder (c : Point) (r : float) (h : float) (tex : Texture) = 
        new HollowCylinder((mkPoint 0.0 0.0 0.0),r,h,tex) |> moveShape c 
    let mkDisc (c : Point) (r : float) (tex : Texture) = new Disc ((mkPoint 0.0 0.0 0.0),r,tex) |> moveShape c
    let mkSolidCylinder (c : Point) (r : float) (h : float) (t : Texture) (top : Texture) (bottom : Texture) 
         = new SolidCylinder((mkPoint 0.0 0.0 0.0),r,h,t,top,bottom) |> moveShape c

    //Triangle
    let mkTriangle a b c mat = new Triangle(a,b,c,(Texture.mkMatTexture mat), None, None) :> Shape

    //Make baseshape for a .ply file
    let mkPLY (filename : string) (smooth : bool) = PLY(parsePly filename, smooth)
    //Make baseshape for an equation
    let mkImplicit s = Bs(mkPoly s)

    //Construct a shape from a baseshape and a texture
    let mkShape (b : BaseShape) (t : Texture) : Shape= 
        match b with
        | PLY(plyList, smooth) -> new TriangleMesh(plyList, t, smooth) :> Shape
        | Bs(p,e) -> (new ImplicitShape(p,e, t)) :> Shape
        | _ -> failwith "Not implemented"