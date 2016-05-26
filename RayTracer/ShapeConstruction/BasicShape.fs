namespace Shapes

open Ray
open Material
open Vector
open Point
open Transformation
open Texture


module BasicShape = 
    let epsilon = 0.0001
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
        member b.getLongestAxis =
                        let xdim = ((Point.getX b.getH) - (Point.getX b.getL), "x")
                        let ydim = ((Point.getY b.getH) - (Point.getY b.getL), "y")
                        let zdim = ((Point.getZ b.getH) - (Point.getZ b.getL), "z") 
                        List.maxBy(fun (x,y) -> x) <| [xdim;ydim;zdim]
        member b.getH =
                b.p2
        member b.getL =
                b.p1
        member b.hit (R(p,d)) =
                //Check intersection between bbox and ray 
            let (ox,oy,oz) = Point.getCoord p
            let (dx,dy,dz) = Vector.getCoord d
            let ((hx,hy,hz), (lx,ly,lz)) = (Point.getCoord b.getH, Point.getCoord b.getL)    
            let (tx,tx') = 
                if dx >= 0.0 then
                   (lx - ox)/dx,  
                   (hx - ox)/dx 
                else 
                   (hx - ox)/dx, 
                   (lx - ox)/dx

            let (ty,ty') = 
                if dy >= 0.0 then
                    (ly - oy)/dy,  
                    (hy - oy)/dy 
                else 
                    (hy - oy)/dy, 
                    (ly - oy)/dy
            let (tz,tz') = 
                if dz >= 0.0 then
                    (lz - oz)/dz,  
                    (hz - oz)/dz 
                else 
                    (hz - oz)/dz, 
                    (lz - oz)/dz
            let t = List.max [tx;ty;tz]
            let t'= List.min [tx';ty';tz']
            if t < t' && t' > 0.0
            then Some (t,t')
            else None

    type Shape = 
         abstract member hit : Ray -> (float * Vector * Material) option
         abstract member isInside : Point -> bool
         abstract member isSolid : unit -> bool
         abstract member getBounding : unit -> BoundingBox option
    

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
                                        Some {p1 = l; p2 = h}

            member this.isSolid() = true
            member this.hit (R(p,d)) = 
                            let makeNV a = Point.move p (a * d) |> Point.direction o
    
                            let calculateMaterial answer = 
                                let nv = makeNV answer
                                let n = nv / r |> normalise
                                
                                let theta = System.Math.Acos(Vector.getY n)
                                let phi' = System.Math.Atan2(Vector.getX n, Vector.getZ n)
                                let phi = if phi' < 0.0 then phi' + (2.0 * pi) else phi'

                                let u, v = phi / (2.0 * pi), 1.0 - (theta/pi)
                               
                                getMaterialAtPoint tex u v
                                
    
                            let a = (Vector.getX d)**2.0 + (Vector.getY d)**2.0 + (Vector.getZ d)**2.0
                            let b =  (2.0 * Point.getX p * Vector.getX d) +
                                        (2.0 * Point.getY p * Vector.getY d) +
                                        (2.0 * Point.getZ p * Vector.getZ d)
                            let c =  (Point.getX p)**2.0 + (Point.getY p)**2.0 +
                                        (Point.getZ p)**2.0 - r**2.0

                            let disc = b**2.0 - (4.0 * a * c)

                            if(disc < 0.0) then None
                            else
                                let answer1 = (-b + System.Math.Sqrt(disc)) / (2.0*a)
                                let answer2 = (-b - System.Math.Sqrt(disc)) / (2.0*a)
                                if answer1 < 0.0 && answer2 < 0.0 then None
                                else
                                    let answer = min answer1 answer2
                                    let material = calculateMaterial answer
                                    if answer < 0.0 
                                    then 
                                        let answer = max answer1 answer2
                                        let material = calculateMaterial answer
                                        Some (answer, makeNV answer, material)
                                    else Some (answer, makeNV answer, material)

    type Plane(tex:Texture) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = None
            member this.isSolid () = false
            member this.hit (R(p,d)) =
                            let dz = Vector.getZ d
                            if dz = 0.0 then None //Check intersection with the plane the Plane is in
                            else let dist = -(Point.getZ p) / dz //Infinite plane
                                 let mat = let hp = Point.move p (dist * d)
                                           let u, v = Point.getX hp, Point.getY hp
                                           Texture.getMaterialAtPoint tex u v
                                 if dist > 0.0 then Some(dist, mkVector 0.0 0.0 1.0, mat)
                                 else None


    type Disc(c:Point, r:float, tex:Texture) =
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = Some {p1 = (mkPoint (Point.getX c - r - epsilon) (Point.getY c - r - epsilon ) (Point.getZ c - epsilon))
                                              ; p2 = (mkPoint (Point.getX c + r + epsilon) (Point.getY c + r + epsilon ) (Point.getZ c + epsilon))}
            member this.isSolid () = false
            member this.hit (R(p,d)) = 
                            let dz = Vector.getZ d
                            if dz = 0.0 then None //Check intersection with the plane
                            else let distance = -(Point.getZ p) / dz
                                 let p' = Point.move p (distance * d)
                                 let (px, py, pz) = Point.getCoord p'
                                 
                                 if (px**2.0 + py**2.0) <= r**2.0 && //Check that intersection is in disc
                                    distance > 0.0
                                 then
                                     let u = (px+r)/(2.0*r)
                                     let v = (py+r)/(2.0*r)
                                     let material = Texture.getMaterialAtPoint tex u v
                                     Some(distance, Vector.mkVector 0.0 0.0 1.0, material)
                                 else None


    type Triangle(a,b,c,tex, texCoords, normals) = 
        let subPoint p1 p2 = let (x1, y1, z1) = getCoord p1
                             let (x2, y2, z2) = getCoord p2
                             (x1-x2,y1-y2,z1-z2)
        member this.getMidPoint () = mkPoint((Point.getX a + Point.getX b + Point.getX c)/3.0) ((Point.getY a + Point.getY b + Point.getY c)/3.0) ((Point.getZ a + Point.getZ b + Point.getZ c)/3.0)
        member this.getCoords s = 
            match s with
            | "x" -> (Point.getX a,Point.getX b,Point.getX c)
            | "y" -> (Point.getY a,Point.getY b,Point.getY c)
            | "z" -> (Point.getZ a,Point.getZ b,Point.getZ c)
            | _ -> failwith "Unknown axis"
        //override t.ToString() = "a: " + a.ToString() + " b: " + b.ToString() + " c: "+ c.ToString()
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = 
                            let xlist = [(Point.getX a);(Point.getX b);(Point.getX c)]
                            let ylist = [(Point.getY a);(Point.getY b);(Point.getY c)]
                            let zlist = [(Point.getZ a);(Point.getZ b);(Point.getY c)]

                            let l = Point.mkPoint((List.min xlist) - epsilon) ((List.min ylist) - epsilon) ((List.min zlist) - epsilon)
                            let h = Point.mkPoint((List.max xlist) + epsilon) ((List.max ylist)+epsilon) ((List.max zlist)+epsilon)
                            Some {p1 = l; p2 = h}

            member this.isSolid () = false
            member this.hit (R(p,d)) = 
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
                              let alpha = 1.0-beta-gamma
                              let t = (a1*(f*l-h*j)+b1*(h*i-e*l)+d1*(e*j-f*i))/D     //z

                              if t > 0.0 then 
                                if beta >= 0.0 && gamma >= 0.0 && gamma+beta <= 1.0 then 

                                     //Calculate the normal 
                                     let n = match normals with
                                             | Some(na, nb, nc) ->   
                                                    let n1 = multScalar na alpha
                                                    let n2 = multScalar nb beta
                                                    let n3 = multScalar nc gamma
                                                    n1+n2+n3 |> normalise
                                             | None -> let u = mkVector1 (subPoint b a)
                                                       let v = mkVector1 (subPoint c a)
                                                       (crossProduct u v) |> normalise 

                                     //Find material for the texture
                                     let mat = match texCoords with //No texture in ply file
                                               | Some((ua, va), (ub, vb), (uc, vc)) ->
                                                    let tu = alpha*ua+beta*ub+gamma*uc
                                                    let tv = alpha*va+beta*vb+gamma*vc
                                                    getMaterialAtPoint tex tu tv
                                                | _ -> getMaterialAtPoint tex 0.5 0.5
                                     //Returns the distance to the hit point, t, the normal of the hit point, and the material of the hit point
                                     Some(t, n, mat)
                                 else None //distance t is smaller than 0
                              else None //gamma + beta is less than 0 or greater than 1
                            else None // Can't divide with zero

    type Rectangle(c,w,h,tex) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = Some {p1 = (mkPoint (getX c - (w/2.0) - epsilon ) (getY c - (h/2.0) - epsilon) (getZ c - epsilon)) 
                                              ; p2 = (mkPoint (getX c + (w/2.0) + epsilon) (getY c + (h/2.0) + epsilon) (getZ c + epsilon))}
            member this.isSolid () = false
            member this.hit (R(p,d)) = 
                            let distance = -(Point.getZ p) / (Vector.getZ d)
                            let p' = Point.move p (distance*d) //Hit point
                            let nV = mkVector 0.0 0.0 1.0 //Normal vector
                            let px, py, pz = getX p', getY p', getZ p' //Hit point coords
                            let ax, ay, az = getX c, getY c, getZ c //Center coords

                            if ax <= px && px <= (ax + w) &&  //Check for intersection
                               ay <= py && py <= (ay + h) && 
                               distance > 0.0
                            then 
                                 let u = (px-ax)/w
                                 let v = (py-ay)/h
                                 let material = getMaterialAtPoint tex u v
                                 Some(distance, nV, material)
                            else None

    type HollowCylinder (center,r,h,tex) = 
        interface Shape with
            member this.isInside p = failwith "Not a solid shape"
            member this.getBounding () = let pLow = mkPoint (getX center - r) (getY center - (h)) (getZ center - r )
                                         let pLow = pLow - epsilon
                                         
                                         let pHigh = mkPoint (getX center + r) (getY center + (h)) (getZ center + r)
                                         let pHigh = pHigh + epsilon
                                         Some {p1 = pLow; p2 = pHigh} 
                                        
            member this.isSolid () = false
            member this.hit (R(p,d)) = 
                            let a = (Vector.getX d)**2.0 + (Vector.getZ d)**2.0
                            let b = (2.0 * Point.getX p * Vector.getX d) + (2.0 * Point.getZ p * Vector.getZ d)
                            let c = (Point.getX p)**2.0 + (Point.getZ p)**2.0 - r**2.0
                            let dis = b**2.0 - (4.0 * a * c)

                                //calculate material
                            let calculateMaterial x y z h r tex =  
                                let n = mkVector (x/r) (0.0) (z/r)
                                let phi' = System.Math.Atan2(Vector.getX n, Vector.getZ n)
                                let phi = if phi' < 0.0 then phi' + 2.0 * pi else phi'
                                let u = phi/(2.0*pi)
                                let v = (y/h) + 0.5
                                let material = Texture.getMaterialAtPoint tex u v
                                material

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
                                let (x, y, z) = move p (tlittle * d) |> getCoord //Hit point coords
                                let material = calculateMaterial x y z h r tex
                                Some(tlittle, Vector.mkVector (px / r) 0.0 (pz / r), material)
                             elif (h / (-2.0)) <= pyt2 && pyt2 <= (h / 2.0) && tbig > 0.0
                             then
                                let px = Point.getX p + tbig * Vector.getX d
                                let pz = Point.getZ p + tbig * Vector.getZ d
                                let (x, y, z) = move p (tbig * d) |> getCoord //Hit point coords
                                let material = calculateMaterial x y z h r tex
                                Some(tbig, Vector.mkVector (px / r) 0.0 (pz / r), material)
                             else None









