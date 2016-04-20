module Sphere

open Shape
open Ray
let pow (x, y) = System.Math.Pow(x, y)
let pi = System.Math.PI

type Shape = Shape.Shape


let hitSphere (R(p,t,d)) (S(o,r,mat)) = 
    let makeNV a = Point.move p (a * d) |> Point.direction o
       
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


