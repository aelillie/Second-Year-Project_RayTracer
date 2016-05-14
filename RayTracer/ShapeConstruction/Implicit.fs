module Implicit
open Point
open Vector
open Ray
open ExprParse
open Material
open ExprToPoly


//A Sphere has the function x^2 + y^2 + z^2 - r^2 = 0
// Implicit Surfaces:
// 1. Parsing
// 2. Plug in the ray equation
// 3. simplify equation
// 4. Find number of roots using Sturms
// 5. Find the roots using Newton

type poly = ExprToPoly.poly
type nVector = Vector.Vector

type baseShape =
  | Bs of poly*expr

let pow (x, y) = System.Math.Pow(x, y)

//helper functions to extract certain types from types
let polyToMap p : Map<int,simpleExpr> =
    match p with
    |Po (x) -> x

let getPoly bs =
    match bs with
    | Bs(p,_) -> p

let getExpr bs =
    match bs with
    | Bs(_,e) -> e


let mkNorm p expr : Vector = 

    //get x,y,z from Point
    let x = Point.getX p
    let y = Point.getY p
    let z = Point.getZ p
    
    //get derived polynomials with respect to x,y,z
    let derivPolyX = polyToMap (exprToPoly expr "x")
    let derivPolyY = polyToMap (exprToPoly expr "y")
    let derivPolyZ = polyToMap (exprToPoly expr "z")

      

    //make list of keys from polyMap
    let listFst m = List.map fst (Map.toList m)
    //get power value as float
    let firstFloat m = float(List.last (listFst m))

    //make a list of values from polyMap
    let listSnd m = List.map snd (Map.toList m)
    //get multiplication value as float
    let secondFloat (m:Map<int,simpleExpr>) = 
                        let se = List.last (listSnd m)
                        let ag = match se with |SE (ag,_) -> ag
                        if ag = [[]] then 1.0
                        else 
                             match se with
                                |SE (ag,agd) -> let ANumLast = List.last ag |> List.last
                                                let ANumDLast = List.last ag |> List.last        
                                                match ANumLast with 
                                                |ANum f -> let d = match ANumDLast with |ANum x -> x |_ -> failwith "Fuck" 
                                                           f
                                                |_ -> failwith "Expected to be ANum"
                       
                               
                         
    let getNew m c = 
                     let x = firstFloat m
                     let k = Map.toList m |> List.map snd
                     let k = secondFloat m 
                     if (firstFloat m)>1.0 then (secondFloat m)*(firstFloat m)*c 
                     else (secondFloat m)*(firstFloat m)

    let checkMap (m:Map<int,simpleExpr>) c = if m.Count>1 then getNew (m) c
                                             else if not (m.ContainsKey(0)) then getNew m c
                                                  else 0.0
    
    let newX = checkMap derivPolyX x
    let newY = checkMap derivPolyY y
    let newZ = checkMap derivPolyZ z


    Vector.mkVector newX newY newZ


let mkImplicit (s : string) (*(constant:string*float)*) : baseShape = 
    let expr = parseStr s
   // let con = FNum (snd constant)

  
    //replace x,y,z with the ray equations corresponding values and 
    let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
    let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
    let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))

   
    let s = ppExpr expr
//     let polX = subst expr ("x", ex)
//    let polY = subst polX ("y", ey)
//    let polyExprSubbed = subst polY ("z", ez)

    let polyExprSubbed = List.fold subst expr [("x",ex);("y",ey);("z",ez)]

    let k = ppExpr polyExprSubbed

    let print = ppPoly "" (exprToPoly polyExprSubbed "t") 
    printfn "%s" print
    //simplify equation 
    Bs ((exprToPoly polyExprSubbed "t"), expr)

    

    
        

    
(*
let hitImplicit (R(p,t,d)) (Bs(pol,s))= 
   

       // SE to atomGroup list (atom list list)
    let getSEList s : atomGroup list = 
        match s with
        | SE (x) -> x

    // map of SE to map of atomGroupList (atom list list)
    let mapToAtomList m = Map.map (fun x y -> getSEList y) m
 
    //substitute atoms with float values: atom list list -> float list list
    let substSE ags = 
        List.map (fun x -> List.map (fun a -> match a with
                                                | AExponent (s,i) -> let sub s = 
                                                                        match s with
                                                                        | "px" -> Point.getX p
                                                                        | "py" -> Point.getY p
                                                                        | "pz" -> Point.getZ p
                                                                        | "dx" -> Vector.getX d
                                                                        | "dy" -> Vector.getY d
                                                                        | "dz" -> Vector.getZ d
                                                                        | _ -> failwith ""
                                                                     pow (sub s,(float i)) 
                                                | ANum c  -> c ) x) ags 

    //Collect the float list list into a single float list
    let collectFloats m = Map.map (fun x y -> List.collect id (substSE y)) m 
    
    //Add the floats in each list of the map     
    let foldMap m = Map.map (fun x y -> List.fold (fun a b -> a+b) 0.0 y) m

    //Poly into Map<int,float>
    let polyMapOfFloats m = (polyToMap >> mapToAtomList >> collectFloats>> foldMap)  m


    let floatMap = polyMapOfFloats pol

    //check what degree of poly we are dealing with, and solve it
    let solveDegreePoly =
        match floatMap.Count with
        | 1 -> failwith "0 degree polynomial doesn't exist"
        | 2 -> failwith ""
        | 3 -> let a = floatMap.Item 2

               let b = floatMap.Item 1

               let c = floatMap.Item 0

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
                            Some (answer, (mkNorm answer p s))
                        else Some (answer, (mkNorm answer p s))
        | 4 -> failwith ""
        | 5 -> failwith ""
        | _ -> failwith ""

    solveDegreePoly
    *)
    
    

                                          
