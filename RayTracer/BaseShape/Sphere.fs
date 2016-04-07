module Shape
open Point
open Vector
open Ray
open ExprParse
open Material

//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0

type Shape =
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
let hit (R(p,t,d)) (s:Shape) =
    match s with
    |S(o,r,mat) ->  let makeNV a = Point.move p (a * d) |> Point.direction o
    
                    let pow (x, y) = System.Math.Pow(x, y)
                    let a = (pow((Vector.getX d),2.0) +
                             pow((Vector.getY d),2.0) +
                             pow((Vector.getZ d),2.0))

                    let b = (2.0 * Point.getX p * Vector.getX d +
                             2.0 * Point.getY p * Vector.getY d +
                             2.0 * Point.getZ p * Vector.getZ d)

                    let c =  pow((Point.getX p),2.0) +
                             pow((Point.getY p),2.0) +
                             pow((Point.getZ p),2.0) -
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
                             Some (answer, makeNV answer, Material.getColour mat)
                            else Some (answer, makeNV answer, Material.getColour mat)
             