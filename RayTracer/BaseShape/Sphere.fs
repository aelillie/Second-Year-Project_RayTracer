module Shape
open Point
open Vector
open Ray
open ExprParse
open Material

//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0

type Shape =
  | S of Point * float * Material
  | P of Point * Vector * Material
  | T of Point * Point * Point * Material
  override s.ToString() =
    match s with
      |S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"
      |P(point,normVector, mat) -> "("+point.ToString()+","+normVector.ToString()+"," + mat.ToString() + ")"
      |T(a,b,c,mat) -> "("+a.ToString()+","+ b.ToString()+","+c.ToString()+","+mat.ToString()+")"

let mkSphere orego radius material = S (orego, radius, material)
let getSphereRadius (S(_,radius,_)) = radius
let getSphereMaterial (S(_, _, mat)) = mat
let mkPlane point normVector material = P (point, normVector, material)
let getPlanePoint (P(point,_,_)) = point
let getPlaneNormVector (P(_,normVector,_)) = normVector
let getPlaneMaterial (P(_, _, mat)) = mat

let mkTriangle a b c mat = T(a,b,c,mat)
let getTriangleA (T(a,_,_,_)) = a
let getTriangleB (T(_,b,_,_)) = b
let getTriangleC (T(_,_,c,_)) = c
let getTriangleMat (T(_,_,_,mat)) = mat


///Given a ray, computes the hit point for a sphere,
//and returns information on how the point
///should be rendered
let hit (R(p,t,d)) (s:Shape) =
    match s with
    |S(o,r,mat) ->  let makeNV a = Point.move p (a * d) |> Point.direction o
    
                    let pow (x, y) = System.Math.Pow(x, y)
                    let a = (pow((Vector.getX d),2.0) +
                             pow((Vector.getY d),2.0) +
                             pow((Vector.getZ d),2.0))

                    let b =  (2.0 * (Point.getX p - Point.getX o) * Vector.getX d) +
                             (2.0 * (Point.getY p - Point.getY o) * Vector.getY d) +
                             (2.0 * (Point.getZ p - Point.getZ o) * Vector.getZ d)

                    let c =  pow((Point.getX p - Point.getX o),2.0) +
                             pow((Point.getY p - Point.getY o),2.0) +
                             pow((Point.getZ p - Point.getZ o),2.0) -
                             pow(r,2.0)

                    let disc = System.Math.Pow(b,2.0) - 4.0 * a * c

                    if(disc < 0.0) then None
                    else
                        let answer1 = (-b + System.Math.Sqrt(disc)) / (2.0*a)
                        let answer2 = (-b - System.Math.Sqrt(disc)) / (2.0*a)
                        if answer1 < 0.0 && answer2 < 0.0 then None
                        else
            
                            let answer = System.Math.Min(answer1,answer2)
                            if answer < 0.0 
                            then 
                             let answer = System.Math.Max(answer1,answer2)
                             Some (answer, makeNV answer, mat)
                            else Some (answer, makeNV answer, mat)

    |P(pVector,n, mat) -> let denom = Vector.dotProduct d n
                          if(denom > 0.0) then
                              let v = Point.distance p pVector
                              let result = Vector.dotProduct v n
                              Some (result, n, mat)
                          else None
    | T(a,b,c,mat) -> 

        let u = Vector.mkVector ((Point.getX b) - (Point.getX a)) ((Point.getY b) - (Point.getY a)) ((Point.getZ b) - (Point.getZ a))
        let v = Vector.mkVector ((Point.getX c) - (Point.getX a)) ((Point.getY c) - (Point.getY a)) ((Point.getZ c) - (Point.getZ a))

        //Function to find the normal of the triangle
        let vectorN a b = Vector.normalise (Vector.crossProduct a b)

        let a1 = (Point.getX a) - (Point.getX b)
        let b1 = (Point.getX a) - (Point.getX c)
        let c1 = Vector.getX d
        let d1 = (Point.getX a) - (Point.getX p)

        let e = (Point.getY a) - (Point.getY b)
        let f = (Point.getY a) - (Point.getY c)
        let g = Vector.getY d
        let h = (Point.getY a) - (Point.getY p)
    
        let i = (Point.getZ a) - (Point.getZ b)
        let j = (Point.getZ a) - (Point.getZ c)
        let k = Vector.getZ d
        let l = (Point.getZ a) - (Point.getZ p)

        let D = a1*(f*k - g*j) + b1*(g*i-e*k) + c1*(e*j-f*i) 

        //Find the unknowns
        //If D!=0 we have a solution    
        if (D <> 0.0)  then 
          let beta = (d1*(f*k-g*j)+b1*(g*l-h*k)+c1*(h*j-f*l))/D  //x
          let gamma = (a1*(h*k-g*l)+d1*(g*i-e*k)+c1*(e*l-h*i))/D //y
          let t = (a1*(f*l-h*j)+b1*(h*i-e*l)+d1*(e*j-f*i))/D     //z
          
          if beta >= 0.0 && gamma >= 0.0 && gamma+beta <= 1.0
           then 
             let p' = Point.move a ((Vector.multScalar u beta) + (Vector.multScalar v gamma))
  
         ///Returns the distance to the hit point, t, the normal of the hit point, and the material of the hit point
             Some(t, vectorN v u, mat)
          else None //gamma + beta is less than 0 or greater than 1
        else None // Can't divide with zero
         