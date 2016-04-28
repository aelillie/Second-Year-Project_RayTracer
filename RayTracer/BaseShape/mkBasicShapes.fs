namespace mkShape

open Ray
open Vector
open System.Drawing
open Point
open Texture
open Material

module mkBasicShapes = 
    let pi = System.Math.PI
    let pow (x, y) = System.Math.Pow(x, y)

    type shape2 =  Ray ->  (float * Vector * Material) option


    let mkSphere (p : Point) (r : float) (m : Material) : shape2 = 
        let hit (R(p,d)) =  let makeNV a = Point.move p (a * d) |> Point.direction p
    
                            let a = (pow((Vector.getX d),2.0) +
                                        pow((Vector.getY d),2.0) +
                                        pow((Vector.getZ d),2.0))

                            let b =  (2.0 * Point.getX p * Vector.getX d) +
                                        (2.0 * Point.getY p * Vector.getY d) +
                                        (2.0 * Point.getZ p * Vector.getZ d)

                            let c =  pow(Point.getX p,2.0) +
                                        pow(Point.getY p,2.0) +
                                        pow(Point.getZ p,2.0) -
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
                                        Some (answer, makeNV answer, m)
                                    else Some (answer, makeNV answer, m)
        hit



    let mkSphereCenter (r : float) (m : Material) : shape2 = mkSphere (mkPoint 0.0 0.0 0.0) r m
    


    let mkPlane mat :shape2 = 
        let pVector = mkPoint 0.0 0.0 0.0
        let n = mkVector 0.0 -1.0 0.0

        let hit (R(p,d))=   let denom = Vector.dotProduct (Vector.normalise d) (Vector.normalise n)
                            if(denom > 0.0000001) then
                                let v = Point.distance p pVector
                                let result = (Vector.dotProduct v n) / denom 
                                if result >= 0.0 then Some (result, n, mat)
                                else None
                            else None
        hit 


    let mkDisc (c : Point) (r : float) (m : Material) = 
        let hit (R(p,d)) =  let dz = Vector.getZ d
                            let pz = Point.getZ p
                            let distance = (-1.0 * pz) / dz
                            let p' = Point.move p (Vector.multScalar d distance)
                            let result = (pow (Point.getX p', 2.0)) + (pow (Point.getY p', 2.0))

                            if result <= (pow (r,2.0)) && distance > 0.0
                            then 
                             Some(distance, Vector.mkVector 0.0 0.0 1.0, m)
                            else 
                             None
        hit

    let mkDiscCenter r t = mkDisc (mkPoint 0.0 0.0 0.0) r t


    let mkRectangle (c : Point) (w : float) (h : float) (m : Material) =
        let hit (R(p,d)) =  let dz = Vector.getZ d
                            let pz = Point.getZ p
                            let distance = (-1.0 * pz) / dz
                            let p' = Point.move p (Vector.multScalar d distance)
    
                            let px = Point.getX p'
                            let py = Point.getY p'
                            let ax = Point.getX c
                            let ay = Point.getY c


                            if ax <= px && px <= (ax + w)  && ay <= py && py <= (ay + h) && distance > 0.0
                            then Some(distance, Vector.mkVector 0.0 0.0 1.0, m)
                            else None
        hit


    let mkTriangle a b c mat : shape2 = 
        let hit (R(p,d)) = 

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
  
                 //Returns the distance to the hit point, t, the normal of the hit point, and the material of the hit point
                 if t > 0.0 
                 then Some(t, vectorN v u, mat)
                 else None
              else None //gamma + beta is less than 0 or greater than 1
            else None // Can't divide with zero

        hit 


    let mkHollowCylinder (center : Point) (r : float) (h : float) (m : Material) =
        let hit (R(p,d)) = 
            let a = pow (Vector.getX d, 2.0) + pow (Vector.getZ d, 2.0)
            let b = (2.0 * Point.getX p * Vector.getX d) + (2.0 * Point.getZ p * Vector.getZ d)
            let c = pow(Point.getX p, 2.0) + pow(Point.getZ p, 2.0) - pow(r, 2.0)
            let dis = pow(b, 2.0) - (4.0 * a * c)

            if dis < 0.0 
            then None
            else 
             let (t1, t2) = (-b + System.Math.Sqrt(dis)) / (2.0*a), (-b - System.Math.Sqrt(dis)) / (2.0*a)
             let (tbig, tlittle) = System.Math.Max(t1,t2), System.Math.Min(t1,t2)
             let pyt1 = Point.getY p + tlittle * Vector.getY d
             let pyt2 = Point.getY p + tbig * Vector.getY d
     

             if (h / (-2.0)) <= pyt1 && pyt1 <= (h / 2.0) && tlittle > 0.0
             then 
                let px = Point.getX p + tlittle * Vector.getX d
                let pz = Point.getZ p + tlittle * Vector.getZ d
                Some(tlittle, Vector.mkVector (px / r) 0.0 (pz / r), m)
             elif (h / (-2.0)) <= pyt2 && pyt2 <= (h / 2.0) && tbig > 0.0
             then
                let px = Point.getX p + tbig * Vector.getX d
                let pz = Point.getZ p + tbig * Vector.getZ d
                Some(tbig, Vector.mkVector (px / r) 0.0 (pz / r), m)
             else None
        hit

    let mkHollowCylinderCenter r h t = mkHollowCylinder (mkPoint 0.0 0.0 0.0) r h t