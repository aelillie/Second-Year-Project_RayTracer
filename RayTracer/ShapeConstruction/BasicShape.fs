namespace Shapes

open Ray
open Material
open Vector
open Point
open Transformation
open Texture


module BasicShape = 
    let epsilon = 0.0000001
    let pi = System.Math.PI
    let pow (x, y) = System.Math.Pow(x, y)
    
    type BoundingBox = 
        {p1 : Point; p2 : Point}
        override b.ToString() = b.p1.ToString() + " " + b.p2.ToString()
        member b.isInside p = 
                        let (x,y,z) = Point.getCoord p
                        let (lx,ly,lz) = Point.getCoord b.p1
                        let (hx,hy,hz) = Point.getCoord b.p2
                        lx < x && x < hx && ly < y && y < hy && lz < z && z < hz 


    type Shape = 
         abstract member hit : Ray -> (float * Vector * Material) option
         abstract member isInside : Point -> bool
         abstract member isSolid : unit -> bool
         abstract member getBounding : unit -> BoundingBox
    

    type Sphere(o:Point, r:float, tex:Texture) = 
        interface Shape with
            member this.isInside p = let (x, y, z) = Point.getCoord p
                                     (x**2.0+y**2.0+z**2.0) < r**2.0
            member this.getBounding () = 
                                        let lx = (Point.getX o) - r - epsilon
                                        let ly = (Point.getY o) - r - epsilon
                                        let lz = (Point.getZ o) - r - epsilon
                                        let l = mkPoint lx ly lz

                                        let hx = (Point.getX o) + r + epsilon
                                        let hy = (Point.getY o) + r + epsilon
                                        let hz = (Point.getZ o) + r + epsilon 
                                        let h = mkPoint hx hy hz
                                        {p1 = l; p2 = h}

            member this.isSolid() = true
            member this.hit (R(p,d)) = 
                            let makeNV a = Point.move p (a * d) |> Point.direction o
                       
                            let calculateMaterial answer = 
//                                let hp = Point.move p (answer * d)
                                let hp = makeNV answer
                                let n = hp / r
                                let n = normalise n
                                
                                let theta = System.Math.Acos(Vector.getY n)
                                let phi' = System.Math.Atan2(Vector.getX n, Vector.getZ n)
                                let phi = if phi' < 0.0 then phi' + (2.0 * pi) else phi'

                                let u = phi / (2.0 * pi)
                                let v = 1.0 - (theta/pi)
                               
                                let material = Texture.getMaterialAtPoint tex u v
                                material
    
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

                            let disc = System.Math.Pow(b,2.0) - (4.0 * a * c)

                            if(disc < 0.0) then None
                            else
                                let answer1 = (-b + System.Math.Sqrt(disc)) / (2.0*a)
                                let answer2 = (-b - System.Math.Sqrt(disc)) / (2.0*a)
                                if answer1 < 0.0 && answer2 < 0.0 then None
                                else
            
                                    let answer = System.Math.Min(answer1,answer2)
                                    let material = calculateMaterial answer
                                    if answer < 0.0 
                                    then 
                                        let answer = System.Math.Max(answer1,answer2)
                                        let material = calculateMaterial answer
                                        Some (answer, makeNV answer, material)
                                    else Some (answer, makeNV answer, material)

    type Plane(tex:Texture) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = false
            member this.hit (R(p,d)) =
                            let pVector = mkPoint 0.0 0.0 0.0
                            let n = mkVector 0.0 -1.0 0.0 
                            let denom = Vector.dotProduct (Vector.normalise d) (Vector.normalise n)
                            if(denom > 0.0000001) then
                                let v = Point.distance p pVector
                                let result = (Vector.dotProduct v n) / denom
                                let getMat a =
                                    let hp = Point.move p (a * d)
                                    let u = abs(Point.getX hp) % 1.0
                                    let v = abs(Point.getY hp) % 1.0
//                                  
                                    Texture.getMaterialAtPoint tex u v

                                 
                                if result >= 0.0 then Some (result, n, getMat result)
                                else None
                            else None
    type Disc(c:Point, r:float, tex:Texture) =
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = false
            member this.hit (R(p,d)) = 
                            let dz = Vector.getZ d
                            let px = Point.getX p
                            let py = Point.getY p 
                            let pz = Point.getZ p
                            let distance = (-1.0 * pz) / dz
                            let p' = Point.move p (Vector.multScalar d distance)
                            let result = (pow (Point.getX p', 2.0)) + (pow (Point.getY p', 2.0))

                            if result <= (pow (r,2.0)) && distance > 0.0
                            then 
                                let u = (px+r)/2.0*r
                                let v = (py+r)/2.0*r
                                let material = Texture.getMaterialAtPoint tex u v
                                Some(distance, Vector.mkVector 0.0 0.0 1.0, material)
                            else None

    type Triangle(a,b,c,mat) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = 
                            let xlist = [(Point.getX a);(Point.getX b);(Point.getX c)]
                            let ylist = [(Point.getY a);(Point.getY b);(Point.getY c)]
                            let zlist = [(Point.getZ a);(Point.getZ b);(Point.getY c)]

                            let l = Point.mkPoint((List.min xlist) + epsilon) ((List.min ylist)+epsilon) ((List.min zlist)+epsilon)
                            let h = Point.mkPoint((List.max xlist) + epsilon) ((List.max ylist)+epsilon) ((List.max zlist)+epsilon)
                            {p1 = l; p2 = h}

            member this.isSolid () = false
            member this.hit (R(p,d)) = 
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

    type Rectangle(c,w,h,tex) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = false
            member this.hit (R(p,d)) = 
                            let dz = Vector.getZ d
                            let pz = Point.getZ p
                            let distance = (-1.0 * pz) / dz
                            let p' = Point.move p (Vector.multScalar d distance)
    
                            let px = Point.getX p'
                            let py = Point.getY p'
                            let ax = Point.getX c
                            let ay = Point.getY c


                            if ax <= px && px <= (ax + w)  && ay <= py && py <= (ay + h) && distance > 0.0
                            then 
                                 let u = (px-ax)/w
                                 let v = (py-ay)/h
                                 let material = Texture.getMaterialAtPoint tex u v
                                 Some(distance, Vector.mkVector 0.0 0.0 1.0, material)
                            else None

    type HollowCylinder (center,r,h,tex) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = failwith "Not implemented"
            member this.isSolid () = false
            member this.hit (R(p,d)) = 
                            let a = pow (Vector.getX d, 2.0) + pow (Vector.getZ d, 2.0)
                            let b = (2.0 * Point.getX p * Vector.getX d) + (2.0 * Point.getZ p * Vector.getZ d)
                            let c = pow(Point.getX p, 2.0) + pow(Point.getZ p, 2.0) - pow(r, 2.0)
                            let dis = pow(b, 2.0) - (4.0 * a * c)

                                //calculate material
                            let calculateMaterial x y z h r tex =  
                                let n = mkVector (x/r) (0.0) (z/r) 
                                let phi' = System.Math.Atan2(Vector.getX n, Vector.getZ n)
                                let phi = if phi' < 0.0 then phi' + 2.0 * pi else phi'
                                let u = phi/(2.0*pi)
                                let v = (y/h) + 0.5
                                let material = Texture.getMaterialAtPoint tex u v
                                material

                            let (px', py', pz') = Point.getCoord p

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
                                let material = calculateMaterial px' py' pz' h r tex
                                Some(tlittle, Vector.mkVector (px / r) 0.0 (pz / r), material)
                             elif (h / (-2.0)) <= pyt2 && pyt2 <= (h / 2.0) && tbig > 0.0
                             then
                                let px = Point.getX p + tbig * Vector.getX d
                                let pz = Point.getZ p + tbig * Vector.getZ d
                                let material = calculateMaterial px' py' pz' h r tex
                                Some(tbig, Vector.mkVector (px / r) 0.0 (pz / r), material)
                             else None









