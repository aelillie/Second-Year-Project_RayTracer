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
open Texture

module ImplicitShape =
    type Poly = ExprToPoly.poly

    type ImplicitShape (pol:Poly, simpleE, t) as this= 
        
        //Polys with respect to each variable used for generating normalvectors for hit point.
        let polX,polY,polZ = 
                              polyToMap (simpleExprToPoly simpleE "x"),
                              polyToMap (simpleExprToPoly simpleE "y"),
                              polyToMap (simpleExprToPoly simpleE "z")
        //Given a values computes the result of a polynomial p given the value x as the value for the variable.
        let calcValue x p = List.fold (fun acc (deg,value)  -> if deg > 0 
                                                                then 
                                                                    acc + (value * (System.Math.Pow(x,(float deg))))
                                                                else 
                                                                    acc + value) 0.0 p
        
        let intervalSize = 100.0    //Arbitrary size.
        //Returns the number of times the sign (+,-) changes through the sturm chain using the interval value.
        let evalInterval (sc:Map<int,float> list) interval =
                        let values = List.map (fun l -> calcValue interval (Map.toList l) ) sc 
                        let rec matchSign x=
                         function 
                            |[] -> x
                            |n1::n2::ns' -> let n1Sign = System.Math.Sign (n1:float)
                                            let n2Sign = System.Math.Sign n2
                                            if n2Sign = 0
                                            then
                                             matchSign (x) (n1::ns')
                                            elif n1Sign <> n2Sign
                                            then
                                             matchSign (x+1) (n2::ns')
                                            else 
                                             matchSign (x) (n2::ns')
                            |_ -> x
                                               
                        matchSign 0 values
        let sturm map = 
                //Computes all Polynomial long divisions of xs1 with xs2
                let pLongDivisions xs1 xs2 = 
                    //Single iteration of polynomial long division
                    let pLongDivision (n:Map<int,float>) (d:Map<int,float>) = 
                        let degree x = fst (List.last (Map.toList  x)) //get largest degree of polynomial x

                        let lead x = List.last (Map.toList x) //Get largest value for x

                        let polyPlus (deg, t1) t2 =           //add a term to the polynomial
                                match Map.tryFind deg t2 with
                                |None ->  Map.add deg t1 t2
                                |Some (x) -> Map.add deg (t1+x) t2

                               

                        let polyMinus t1 t2 =               //Subtracting 2 polys.
                                let minus x1 x2 =           
                                          match x1 with 
                                           |None ->  x2
                                           |Some t -> x2 - t
                                        
                                let t1' = Map.toList t1
                                List.map (fun (deg, t) -> let x = Map.tryFind deg t2 in (deg, minus x t) ) t1'
                                |> Map.ofList               
                            
                        //Multiply a polynomial with a term
                        let polyMult (deg, t1:float) t2 = 
                                Map.fold (fun acc key value -> (Map.add (key+deg) (value*t1) acc)) Map.empty t2
                        
                        //Divide to terms
                        let polyDivide (deg1, t1:float) (deg2 , t2:float) =
                                (deg1 - deg2, t1/t2)
 
                        let removeZero r =          //Remove if zero (Or really close to!)
                            Map.filter (fun key value -> ((abs value) > 0.0000000000000001) ) r
                           
                        let q = Map.empty
                        let r = n

                        //The computation of kvotient and remainder
                        let rec calcQR (q, (r:Map<int,float>)) d =
                            match Map.isEmpty r with    //Map empty meaning Quotient and remainder has been found
                            |true -> (q , r)
                            |_ -> if (degree r) >= (degree d) //If criteria is met, follow the algorithm for finde Q and R
                                  then                                     
                                   let (deg,t) = polyDivide (lead r) (lead d)
                                   let q = polyPlus (deg, t) q
                                   let x = polyMult (deg, t) d
                                   let r' = polyMinus r x |> removeZero
                                   calcQR (q,r') d //Recursively find Q and R
                                  else (q, r)
                        calcQR (q,r) d
                    
                    //Addition of 2 polynomials represented as maps
                    let plusPolyMaps (m1:Map<int,float>) (m2:Map<int,float>) =

                        let merge a  b  =
                            Map.fold (fun s k v ->
                            match Map.tryFind k s with
                            | Some v' -> Map.add k (v + v') s
                            | None -> Map.add k v s) a b
                        merge m1 m2

                    let negatePolyMaps m = 
                        Map.map (fun key value -> value * (-1.0)) m
                    
                    //Check if only contains constants.
                    let onlyConstant (m:Map<_,_>) = 
                        if m.Count = 1 && m.ContainsKey 0 
                        then true
                        else false
                    //Creates the list of Remainders from doing polynomial long division on 2 polynomials                            
                    let rec pLong p px xs = 
                        if Map.isEmpty px 
                        then
                         xs
                        elif onlyConstant px
                        then 
                         xs
                        else
                         let (q, rem) = pLongDivision p px
                         let px' = negatePolyMaps rem
                         pLong px px' (px'::xs)
                          
                         
                    pLong xs1 xs2 List.empty


                //Derives a polynomial
                let derive (m:Map<int,float>) : Map<int,float> =
                    let m' =  (List.tail (Map.toList m))
                    let m' = List.map (fun (d, n) -> (d-1, n * (float d))) m'
                    Map.ofList m' 
                //Create the 2 first sequences of the chain separately
                let p0 = map
                let p1 = derive map

                let sturmChain = List.append [p0;p1] (List.rev (pLongDivisions p0 p1)) 
                List.filter (fun x -> not (Map.isEmpty x)) sturmChain

        
        let newtRaph sturmChain = 
            let nOfRoots iStart iEnd = let positive = evalInterval sturmChain iEnd
                                       let negative = evalInterval sturmChain iStart
                                       negative-positive
                                           
            if nOfRoots 0.0 100.0> 0 
             then  //converge on interval by halving the interval, and look in either the first half or the 2nd half, depending on where the root is                                                                              
                let rec getInterval s e count prev : (float*float) = 
                    if count > 0 then
                     let roots = nOfRoots s e
                     if roots > 0 then                                                                                                                                                                                                      
                                   getInterval s ((e+s)/2.0) (count-1) e
                     else
                          getInterval e prev (count-1) prev
                    else (s,e)
                                                                                                      
                let intv = getInterval 0.0 100.0 15 0.0
                let iStart = fst intv                                            
                let iEnd = snd intv
               
                //get the function and the derived function
                let fNorm = (Map.toList (List.item 0 sturmChain))
                let fPrime = (Map.toList (List.item 1 sturmChain))

                //calculate a more accurate guess based on an initial guess                                         
                let rec calcGuess guess rc = if rc > 0 then                                                                      
                                               let newGuess = guess - ((calcValue guess fNorm )/(calcValue guess  fPrime))                                                                                                                                                           
                                               calcGuess newGuess (rc-1)
                                             else guess

                //converge on the root giving a guess based on the interval                                              
                calcGuess ((iEnd+iStart)/2.0) 6
            else 0.0   
        
        let findHit (p:Point) (d:Vector) floatmap se = 
            let root = newtRaph (sturm floatmap)                                           
            if root = 0.0 || root < 0.0 then None
            else                                             
              let hitPoint = Point.move p (root*d)
          
              Some (root, Vector.normalise(mkNorm hitPoint (polX,polY,polZ)),(Texture.getMaterialAtPoint t 0.0 0.0))  


        
                 
        interface Shape with
            member this.getBounding () = None
            member this.isInside p = failwith "Not implemented"
            member this.isSolid () = true
            member this.hit (R(p,d) as ray) = 
                                let getSEList s = 
                                    match s with
                                    | SE (ag, ags) -> (ag,ags)


                                // map of SE to map of atomGroupList (atom list list)
                                let mapSEToAtomGroups m = Map.map (fun x y -> getSEList y) m
 
                                //substitute atoms with float values: atom list list -> float list list
                                let substAtomG ags = 
                                    List.map (fun x -> List.map (fun a -> match a with
                                                                            | AExponent (s,i) -> let sub = 
                                                                                                    match s with
                                                                                                    | "px" -> Point.getX p
                                                                                                    | "py" -> Point.getY p
                                                                                                    | "pz" -> Point.getZ p
                                                                                                    | "dx" -> Vector.getX d
                                                                                                    | "dy" -> Vector.getY d
                                                                                                    | "dz" -> Vector.getZ d
                                                                                                    | _ ->  failwith ""
                                                                                                 pow (sub,(float i)) 
                                                                            | ANum c  -> c ) x) ags 

                                //Collect the float list list into a single float list
                                let subFloats m = Map.map (fun x (y,d) -> (substAtomG y, substAtomG d) ) m

                                let multFloats m = Map.map (fun x (y,d) -> let y' = List.map (fun ys -> List.fold (fun a b -> a*b) 1.0 ys) y
                                                                           let d' = List.map (fun ds -> List.fold (fun a b -> a*b) 1.0 ds) d
                                                                           (y',d')) m
                                
                                

                                let divideFloats m = 
                                                    Map.map (fun x (y,d) -> (List.map2 (fun t d -> t/d) y d)) m
                                //let collectFloats m = Map.map (fun x y -> List.collect id (substSE y)) m 
    
                                //Add the floats in each list of the map     
                                let foldMap m = Map.map (fun x y -> let ys = List.fold (fun a b -> a+b) 0.0 y
                                                                    (ys)) m


                                let polyMapOFloats m = (polyToMap >> mapSEToAtomGroups >> subFloats >> multFloats >> divideFloats >> foldMap )  m

                                
                                let floatMap = polyMapOFloats pol

                                //check what degree of poly we are dealing with, and solve it
                                let solveDegreePoly =
                                    match floatMap.Count with
                                    | 1 -> failwith "0 degree polynomial doesn't exist"
                                    | 2 -> let a = floatMap.Item 1

                                           let b = floatMap.Item 0                               
                                            
                                           if a=0.0 then None
                                           else let res = (-b)/a
                                           
                               
                                                let hitPoint = Point.move p (res*d)
                                                let nVector = Vector.normalise(mkNorm hitPoint (polX,polY,polZ))
                                                let denom = Vector.dotProduct  d nVector 
                                                if -denom<0.000001 then None
                                                //else if res < 0.0 then None
                                                else Some (res, nVector, (Texture.getMaterialAtPoint t 0.0 0.0))

                                    | 3 ->  
                                            let a = floatMap.Item 2

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
                                                        let nV = Point.direction (mkPoint 0.0 0.0 0.0) nvPointMax

                                                        Some (answer, Vector.normalise(nV),(Texture.getMaterialAtPoint t 0.0 0.0)) 
                                                        
                                                    //else Some (answer, (mkNorm nvPointMin nvExpr),m)
                                                    else
                                                        let nV = Point.direction (mkPoint 0.0 0.0 0.0) nvPointMin 
                                                        Some (answer, Vector.normalise(nV),(Texture.getMaterialAtPoint t 0.0 0.0)) 
                                           
                                    | _ -> findHit p d floatMap simpleE 


                                solveDegreePoly
                                           
               
        

