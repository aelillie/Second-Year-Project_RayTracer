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


let pow (x, y) = System.Math.Pow(x, y)

//helper functions to extract certain types from types
let polyToMap p : Map<int,simpleExpr> =
    match p with
    |Po (x) -> x




let mkNorm p expr : Vector = 

    //get x,y,z from Point
    let x = Point.getX p
    let y = Point.getY p
    let z = Point.getZ p
    
    //get derived polynomials with respect to x,y,z
    let derivPolyX = polyToMap (exprToPoly expr "x")
    let derivPolyY = polyToMap (exprToPoly expr "y")
    let derivPolyZ = polyToMap (exprToPoly expr "z")

    //Function that derives a polynomial with only floats, reprenseted as a map.
    let Derive m = Map.fold (fun acc degree value -> if degree = 0
                                                     then Map.add 0 0.0 acc 
                                                     elif degree = 1
                                                     then Map.add 0 value acc
                                                     else Map.add (degree-1) (value * float degree) acc ) Map.empty m 
                                                     
     
    //Substitutes variables with their values. Also removes division by dividing.                                                 
    let toFloat se = match se with
                     |SE (agl,agd) -> let rec agF ag s =
                                        match ag with
                                         |[] -> s
                                         |a::ag'-> let k = match a with
                                                            |ANum f -> f * s
                                                            |AExponent(e, n) when e = "x" -> pow (x, float n) * s
                                                            |AExponent(e, n) when e = "y" -> pow (y, float n) * s
                                                            |AExponent(e, n) when e = "z" -> pow (z, float n) * s
                                                            |_ -> failwith "Unexpected variable when finding normal vector"
                                                   agF ag' k
                                                             
                                         
                                      List.fold2 (fun acc x y  -> acc + ((agF x 1.0) / (agF y 1.0))) 0.0 agl agd
                                        
                          
    //A Computes the derived polynomial, by first substitute values.
    let derivePoly c m = let k = Map.map (fun key value -> toFloat value) m |> Derive 
                         Map.fold (fun acc key value -> acc + (value * pow(c,float key))) 0.0  k

    let x' = derivePoly x derivPolyX //Derive with respect to variables.
    let y' = derivePoly y derivPolyY
    let z' = derivePoly z derivPolyZ


    Vector.mkVector x' y' z'


let mkPoly (s : string) (*(constant:string*float)*)  = 
    let expr = parseStr s
   // let con = FNum (snd constant)

  
    //replace x,y,z with the ray equations corresponding values and 
    let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
    let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
    let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))

   
    let s = ppExpr expr
    printfn "%s\n" s
//     let polX = subst expr ("x", ex)
//    let polY = subst polX ("y", ey)
//    let polyExprSubbed = subst polY ("z", ez)

    let polyExprSubbed = List.fold subst expr [("x",ex);("y",ey);("z",ez)]

    let k = ppExpr polyExprSubbed

    let k = exprToPoly polyExprSubbed "t"
    let print = ppPoly "" (exprToPoly polyExprSubbed "t") 
    printfn "%s\n" print
    //simplify equation 
    (k), expr

    

                        
