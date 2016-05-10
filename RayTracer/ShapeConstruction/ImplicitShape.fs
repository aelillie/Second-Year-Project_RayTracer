namespace Shapes


open Ray
open Material
open Vector
open Point
open Transformation
open BasicShape
open TransformedShape
open PlyParse
open ExprParse
open ExprToPoly
open Implicit


module ImplicitShape =
    

    type ImplicitShape (bs, m) = 
        interface Shape with
            member this.getBounding () = failwith "Not Imlemented"
            member this.isInside p = failwith "Not implemented"
            member this.isSolid () = failwith "Not implemented"
            member this.hit (R(p,d) as ray) = 
                                let getSEList s : atomGroup list = 
                                    match s with
                                    | SE (x) -> x

                  
                                let pol = getPoly bs
                                let expr = getExpr bs

                    
                                // map of SE to map of atomGroupList (atom list list)
                                let mapToAtomList m = Map.map (fun x y -> getSEList y) m
 
                                //substitute atoms with float values: atom list list -> float list list
                                let substSE ags = 
                                    List.map (fun x -> List.map (fun a -> match a with
                                                                            | AExponent (s,i) -> let sub = 
                                                                                                    match s with
                                                                                                    | "px" -> Point.getX p
                                                                                                    | "py" -> Point.getY p
                                                                                                    | "pz" -> Point.getZ p
                                                                                                    | "dx" -> Vector.getX d
                                                                                                    | "dy" -> Vector.getY d
                                                                                                    | "dz" -> Vector.getZ d
                                                                                                    | _ -> failwith ""
                                                                                                 pow (sub,(float i)) 
                                                                            | ANum c  -> c ) x) ags 

                                //Collect the float list list into a single float list
                                let subFloats m = Map.map (fun x y -> substSE y) m

                                let multFloats m = Map.map (fun x y -> List.map (fun ys -> List.fold (fun a b -> a*b) 1.0 ys) y) m
                   
                                //let collectFloats m = Map.map (fun x y -> List.collect id (substSE y)) m 
    
                                //Add the floats in each list of the map     
                                let foldMap m = Map.map (fun x y -> List.fold (fun a b -> a+b) 0.0 y) m

                                //Poly into Map<int,float>
                                let polyMapOFloats m = (polyToMap >> mapToAtomList >> subFloats >> multFloats >> foldMap)  m


                                let floatMap = polyMapOFloats pol

                                //check what degree of poly we are dealing with, and solve it
                                let solveDegreePoly =
                                    match floatMap.Count with
                                    | 1 -> failwith "0 degree polynomial doesn't exist"
                                    | 2 -> let a = floatMap.Item 1

                                           let b = floatMap.Item 0                               
                               
                                           let res = (-b)/a
                               
                                           let hitPoint = Point.move p (res*(Vector.normalise(d)))
                                           let nVector = mkNorm hitPoint expr
                                           let denom = Vector.dotProduct d nVector                                        
                                           if denom<0.0 then None
                                            //if res < 0.0 then None
                                           else                                  
                                                Some (res, nVector, m)

            //                               if(denom < 0.0) then none
            //                               else
            //                                    let v = Point.distance p       
                                                

            //                               let denom = Vector.dotProduct d n
            //                               if(denom > 0.0) then
                  //                              let v = Point.distance p pVector
                    //                            let result = Vector.dotProduct v n
            //                                    Some (result, n, mat)
            //                               else None
                                    | 3 ->  let a = floatMap.Item 2

                                            let b = floatMap.Item 1

                                            let c = floatMap.Item 0

                                            let disc = System.Math.Pow(b,2.0) - (4.0 * a * c)                              
                                
                                            if(disc < 0.0) then None
                                            else
                                                let answer1 = (-b + System.Math.Sqrt(disc)) / (2.0*a)
                                                let answer2 = (-b - System.Math.Sqrt(disc)) / (2.0*a)
                                  
                                                if answer1 < 0.0 && answer2 < 0.0 then None
                                                else
                                                    let answer = System.Math.Min(answer1,answer2)
                                                    //normal vector point with minimum answer value
                                                    let nvPointMin = Point.move p (answer * d)
                                                    if answer < 0.0 
                                                    then 
                                                        let answer = System.Math.Max(answer1,answer2)
                                                        //normal vector point with maximum answer value
                                                        let nvPointMax = Point.move p (answer * d)
                                                        Some (answer, Vector.normalise(mkNorm nvPointMax expr),m) 
                                                    //else Some (answer, (mkNorm nvPointMin nvExpr),m)
                                                    else Some (answer, Vector.normalise(mkNorm nvPointMin expr),m) 
                                    | 4 -> failwith "3rd degree"
                                    | 5 -> failwith "4th degree"
                                    | _ -> failwith "degree over 9000"

                                solveDegreePoly

