module ExprToPoly

(*#load "ExprParse.fs"*)

open ExprParse
type expr = ExprParse.expr

let rec ppExpr = function
  | FNum c -> string(c)
  | FVar s -> s
  | FAdd(e1,e2) -> "(" + (ppExpr e1) + " + " + (ppExpr e2) + ")"
  | FMult(e1,e2) -> (ppExpr e1) + " * " + (ppExpr e2)
  | FExponent(e,n) -> "(" + (ppExpr e) + ")^" + string(n)
  | FRoot(e,n) -> "(" + (ppExpr e) + ")_" + string(n)
  | FDiv(e1,e2) -> (ppExpr e1) + " / " + (ppExpr e2)

//The expressions is a representation of e.g. a Sphere
//of the type expr, given from ExprParse
let rec subst e (x,ex) = //expression (variable to replace, substitution)
  match e with    
  | FNum c          -> FNum c //No substitution of numbers
  | FVar s          -> if x = s then ex else FVar s
  | FAdd(e1,e2)     -> FAdd(subst e1 (x,ex), subst e2 (x,ex))
  | FMult(e1,e2)    -> FMult(subst e1 (x,ex), subst e2 (x,ex))
  | FExponent(z, n) -> FExponent(subst z (x,ex), n)
  | FRoot(e,n) -> FRoot(subst e (x,ex), n)
  | FDiv(e1,e2) -> FDiv(subst e1 (x,ex), subst e2 (x,ex))


 
//let rec Derivative x : expr =
//    match x with
//    | FNum c -> FNum 0.0
//    | FVar s -> FNum 1.0
//    | FAdd(e1, e2) -> FAdd(Derivative(e1), Derivative(e2))
//    | FExponent(FNum c, n) -> FNum 0.0
//    | FMult(e1, e2) -> FAdd(FMult(Derivative(e1), e2), FMult(e1, Derivative(e2))) 
//    | FExponent(e, n) -> FMult(FNum (float(n)), FExponent(e, n-1))
 
    
//let rec exprsToDivs = function
//    | FNum c -> FNum c
//    | FVar s -> FVar s
//    | FAdd(e1,e2) -> FAdd(FDiv(exprsToDivs e1,FNum 1.0), FDiv(exprsToDivs e2, FNum 1.0))
//    | FMult(e1,e2) -> FMult(FDiv(exprsToDivs e1,FNum 1.0), FDiv(exprsToDivs e2, FNum 1.0))
 



    
//a number or a variable to some power
//Single variable, x, is represented as AExponent(x,1)
type atom = ANum of float | AExponent of string * int
type atomGroup = atom list //implicitly multiplied atoms
type simpleExpr = SE of atomGroup list //implicitly added atom groups
let isSimpleExprEmpty (SE ags) = ags = [] || ags = [[]]




let ppAtom = function
  | ANum c -> string(c)
  | AExponent(s,1) -> s
  | AExponent(s,n) -> s+"^"+(string(n))
let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

//multiply all components and eliminate parantheses
let rec combine xss = function
  | [] -> []
  | ys::yss -> List.map ((@) ys) xss @ combine xss yss


//let rec combDivide xss = function
//    | [] -> []
//    | ys::yss -> List.map ( fun xs -> ADivision(ys,xs)) xss @ combDivide xss yss

//Simplify an expression into a simpleExpr (Use table on p. 2)
let rec simplify = function
  | FNum c          -> [[ANum c]]
  | FVar s          -> [[AExponent(s,1)]]
  | FAdd(e1,e2)     -> simplify e1 @ simplify e2
  | FMult(e1,e2)    -> combine (simplify e1) (simplify e2)
  | FExponent(e1,0) -> [[ANum 1.0]]
  | FExponent(e1,1) -> simplify e1
  | FExponent(e1,n) when n < 0 -> simplify (FDiv(FNum 1.0, FExponent(e1,System.Math.Abs(n))))
  | FExponent(e1,n) -> simplify (FMult(e1, FExponent(e1, n-1)))
  | FDiv(e1,e2) -> failwith""
  | FRoot(e,n) -> simplify (FExponent(e,1/n))  


//reduces duplication, so x*x becomes x^2
//implicitly multiplied atoms
let simplifyAtomGroup ag = 
    let folder (m, v) elem =
        match elem with
        | ANum n  //accumulate numbers
            -> (m, v * n)
        | AExponent (s, n) //Map variables to their exponent
            -> let value = if Map.containsKey s m //update if exists
                           then (Map.find s m) + n else n
               let m' = Map.add s value m in (m', v)
    let (table, num) = 
        if List.isEmpty ag
        then 
         (Map.empty, 0.0)
        else 
         List.fold folder (Map.empty, 1.0) ag
        
    let mapList = Map.toList table //convert map to list
    let AExList = mapList |> List.map (fun (s, f) -> AExponent (s,f))
    match (num, AExList) with
    | (0.0, AExponent (s,f) :: xs)  -> [] //0 * expr is 0
    | (0.0, [])                     -> [] //No need to keep the 0
    | (1.0, AExponent (s,f) :: xs)  -> AExList //1 * expr is expr
    | (1.0, [])                     -> [ANum 1.0] //Constants must remain
    | (num, atomGroup)              -> ANum num :: AExList

//implicitly added atom groups
let simplifySimpleExpr (SE ags) =
  //simplify each atom group
  let ags' = List.map simplifyAtomGroup ags
  // Add atom groups with only constants together.
  let cFolder (a, s) elem =
        match (a, elem) with
        | ([ANum (n)], [ANum (f)]) -> ([ANum (f+n)], s) //acc numbers
        | _ -> (a, elem :: s) //just cons rest of ag'
  let (agConst, ags'') = List.fold cFolder ([ANum (0.0)], []) ags'
  // Last task is to group similar atomGroups into one group
  let eFolder map elem =  //map ag to their number of appearance
        
        let count = if Map.containsKey elem map //update if exists
                    then (Map.find elem map) + 1.0 else 1.0 
        if List.isEmpty elem 
        then map
        else Map.add elem count map
  let agSimMap = List.fold eFolder Map.empty ags''
  let filterAg (s, f) = if f <> 1.0 && f <> 0.0 
                        then (ANum (f)) :: s else s
  let agSim = (Map.toList agSimMap) |> List.map filterAg
  if agConst.Head = ANum (0.0) then SE (agSim) //dispose 0s
  else SE (agConst :: agSim) 

let exprToSimpleExpr e = simplifySimpleExpr (SE (simplify e))

type poly = Po of Map<int,simpleExpr>

let ppPoly v (Po p) =
  let pp (d,ags) =
    let prefix = if d=0 then "" else ppAtom (AExponent(v,d))
    let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
    prefix + postfix
  String.concat "+" (List.map pp (Map.toList p))

let removeV v = function
    | AExponent (s,f) -> s <> v
    | _ -> true

(* Collect atom groups into groups with respect to one variable v *)
let splitAG v m = function
  | [] -> m
  | ag ->
    let eqV = function AExponent(v',_) -> v = v' | _ -> false
    let addMap d ag m = 
        let agTrimmed = List.filter (removeV v) ag
        if Map.containsKey d m
        then match Map.find d m with
             | SE agList -> Map.add d (SE (agTrimmed :: agList)) m
        else Map.add d (SE [agTrimmed]) m
    match List.tryFind eqV ag with
      | Some (AExponent(_,d)) ->
        let ag' = List.filter (not << eqV) ag
        addMap d ag' m
      | Some _ -> failwith "splitAG: Must never come here! - ANum will not match eqV"
      | None -> addMap 0 ag m

let simpleExprToPoly (SE ags) v =
  Po (List.fold (splitAG v) Map.empty ags)

let exprToPoly e v = (exprToSimpleExpr >> simplifySimpleExpr >> simpleExprToPoly) e v
