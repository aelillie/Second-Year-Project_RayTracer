module Shape
open Point
open Vector
open Ray
open ExprParse
open Material

//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0

type Sphere =
  | S of Point * float * Material
  override s.ToString() =
    match s with
      S(orego,radius, mat) -> "("+orego.ToString()+","+radius.ToString()+"," + mat.ToString() + ")"

let mkSphere orego radius material = S (orego, radius, material)
let getRadius (S(_,radius,_)) = radius
let getMaterial (S(_, _, mat)) = mat

///Given a ray, computes the hit point for a sphere,
//and returns information on how the point
///should be rendered
let sphereHit (R(p,t,d)) (S(o,r, mat)) =
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
            let makeNV a = Point.move p (a * d) |> Point.direction o
            if  answer1 >= answer2 
            then Some (answer2, makeNV answer2, Material.getColour mat)
            else Some (answer1, makeNV answer1, Material.getColour mat) 