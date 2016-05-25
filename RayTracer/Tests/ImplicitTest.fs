module ImplicitTest
open ExprParse
open ExprToPoly
open Implicit
open Shapes.TransformedShape
open Shapes.BasicShape
open Ray
open Point
open Vector
let chk (name,res1,res2) =
  printf "%s %s\n" name (if res1 = res2 then "OK" else "FAILED\nActual:\n"+(string)res1+"\nExpected:\n"+(string)res2+"\n")

let toFloatMap pol (R(p,d)) =
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
                                                                        | _ -> failwith ""
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

                                
    polyMapOFloats pol





let chmutov degree =
    let factorial x = 
      if x = 0 then 1 else
      let rec fac_aux a acc =
        if a >= x then
          a * acc
        else
          fac_aux (a + 1) (a * acc)
      fac_aux 1 x

    let comb a b = 
      let x = float (factorial a) in
      let y = float (factorial b) in
      let z = float (factorial (a - b)) in
        x / (y * z)

    let rec strSum n f : string =
      if n = 0 then
        f 0
      else
        f n + " + " + (strSum (n - 1) f)
       
    let T x = strSum (degree / 2) (fun (k : int) -> (string (comb degree (2 * k))) + " * (" + x + "^2 + -1.0)^" + (string k) + " * " + x + "^" + (string (degree - (2 * k))))
    T "x" + " + " + T "y" + " + " + T "z"
    
    

let doTest() = 
    let chmutov2 = chmutov 2
    let chmutov4 = chmutov 4


    let expr = parseStr chmutov2

    let simple = ExprToPoly.exprToSimpleExpr expr
    let exprSubst =   let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
                      let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
                      let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))
                      List.fold subst expr [("x",ex);("y",ey);("z",ez)]
    let simpleSubst = exprToSimpleExpr exprSubst

    let poly = simpleExprToPoly simpleSubst "t"

    let floatMap = toFloatMap poly (R((mkPoint 1.0 1.0 3.0), (mkVector 0.3 (-0.4) (-0.67))))
    let c = Map.find 0 floatMap
    let first = Map.find 1 floatMap
    let sec = Map.find 2 floatMap 

    let tests =
     [("01 FloatMap Constant", (string) c, (string) 19.0); 
     ("02 FloatMap 1st degree", (string) first, (string) -8.44);
     ("03 FloatMap 2nd Degree", (string) sec, (string) 1.3978)
     ]

    printf "ImplicitTest\n"
    List.iter chk tests


