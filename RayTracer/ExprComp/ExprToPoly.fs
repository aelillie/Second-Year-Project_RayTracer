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
//    | FDiv(e1,e2) -> FDiv(exprsToDivs e1, exprsToDivs e2)

        
//let rec exprsToDivs = function
//    | FNum c -> FDiv(FNum c, FNum 1.0)
//    | FVar s -> FDiv(FVar s, FNum 1.0)
//    | FAdd(e1,e2) -> FAdd(exprsToDivs e1, exprsToDivs e2)
//    | FMult(e1,e2) -> FMult(exprsToDivs e1, exprsToDivs e2)
//    | FExponent(e,n) -> FDiv(FExponent(exprsToDivs e,n),FNum 1.0)
//    | FRoot(e,n) -> FDiv(FRoot(exprsToDivs e,n),FNum 1.0)
//    | FDiv(e1,e2) -> FDiv(exprsToDivs e1,exprsToDivs e2)

//let rec simpDivs = function
//    | FNum c -> FNum c
//    | FVar s -> FVar s
//    | FAdd(FDiv(e1,e2),FDiv(e3,e4)) when e2 = e4 -> FDiv(FAdd(e1,e2),e3) 
//    | FMult(FDiv(e1,e2),FDiv(e3,e4)) -> FDiv(FMult(simpDivs e1, simpDivs e3),FMult(simpDivs e2, simpDivs e4))
//    | FAdd(FDiv(e1,e2),FDiv(e3,e4)) -> FDiv(FAdd(FMult(simpDivs e1,simpDivs e4),FMult(simpDivs e3, simpDivs e2)),FMult(simpDivs e2,simpDivs e4))
//    | FDiv(FDiv(e1,e2),FDiv(e3,e4)) -> FDiv(FMult(simpDivs e1, simpDivs e4),FMult(simpDivs e3, simpDivs e2))
//    | FAdd(e1,e2) -> FDiv(simpDivs e1, simpDivs e2)
//    | FMult(e1,e2) -> FDiv(simpDivs e1, simpDivs e2)
//    | FDiv(e1,e2) -> FDiv(simpDivs e1, simpDivs e2)
//    | FExponent(e,n) -> FExponent(simpDivs e,n)
//    | FRoot(e,n) -> failwith "Should only be Adds, Mults and Divs"
// 
 


    
//a number or a variable to some power
//Single variable, x, is represented as AExponent(x,1)
type atom = ANum of float | AExponent of string * int 
type atomGroup = atom list //implicitly multiplied atoms
type simpleExpr = SE of atomGroup list * atomGroup list  //implicitly added atom groups
let isSimpleExprEmpty (SE (ags,ags')) = ags = [] || ags = [[]]




let ppAtom = function
  | ANum c -> string(c)
  | AExponent(s,1) -> s
  | AExponent(s,n) -> s+"^"+(string(n))
let ppAtomGroup ag agd = "( " + (String.concat "*" (List.map ppAtom ag)) + " / " + (String.concat "*" (List.map ppAtom agd)) + " )"
                         
let ppSimpleExpr (SE (ags, ags')) = 
                                    String.concat "+" (List.map2 ppAtomGroup ags ags' )

//multiply all components and eliminate parantheses
let rec combine xss = function
  | [] -> []
  | ys::yss -> List.map ((@) ys) xss @ combine xss yss




//let rec combDivide xss = function
//    | [] -> []
//    | ys::yss -> List.map ( fun xs -> ADivision(ys,xs)) xss @ combDivide xss yss

//Simplify an expression into a simpleExpr (Use table on p. 2)
//let rec simplify = function
//  | FNum c          -> [[ADiv([[ANum c]],[[ANum 1.0]])] ]
//  | FVar s          -> [[ADiv([[AExponent(s,1)]],[[ANum 1.0]])]]
//  | FAdd(FNum c, FNum c2) -> [[ADiv([[ANum (c+c2)]],[[ANum 1.0]])]]
//  | FAdd(e1,e2)     -> simplify e1 @ simplify e2
//  | FMult(e1,e2)    -> combine (simplify e1) (simplify e2)
//  | FExponent(e1,0) -> [[ADiv([[ANum 1.0]],[[ANum 1.0]])]]
//  | FExponent(e1,1) -> [[ADiv(simplify e1,[[ANum 1.0]])]]
//  | FExponent(e1,n) -> [[ADiv(simplify (FMult(e1, FExponent(e1, n-1))),[[ANum 1.0]])]]
//  | FDiv(e1,e2) -> [[ADiv(simplify e1, simplify e2)]]
  //| FRoot(e,n) -> simplify (FExponent(e,1/n))
  //| FExponent(e1,n) when n < 0 -> [[ADiv(simplify (FDiv(FNum 1.0, FExponent(e1,System.Math.Abs(n))))

let rec simplify = function
  | FNum c          -> [[ANum c]] 
  | FVar s          -> [[AExponent(s,1)]]
  | FAdd(e1,e2)     -> simplify e1 @ simplify e2
  | FMult(e1,e2)    -> combine (simplify e1) (simplify e2)
  | FExponent(e1,0) -> [[ANum 1.0]]
  | FExponent(e1,1) -> simplify e1
  | FExponent(e1,n) when n < 0 -> simplify (FDiv(FNum 1.0, FExponent(e1,System.Math.Abs(n))))
  | FExponent(e1,n) -> simplify (FMult(e1, FExponent(e1, n-1)))
//  | FDiv(FNum x, FNum y) -> [[ANum (x/y)]]
//  | FDiv(FVar s, FNum c) -> [[ANum(1.0/c);AExponent (s,1)]]
  //| FDiv(e1,e2) -> combDiv (simplify e1) (simplify e2)
  | FRoot(e,n) -> simplify (FExponent(e,1/n))
  | FDiv(e1, e2) -> simplify e1


let rec simplifyDivisor = function
  | FDiv(FNum x, FNum y) -> [[ANum y]]
  | FDiv(FVar s, FNum c) -> [[ANum c]]
  | FDiv(FMult(e1 ,e2), e3) -> simplifyDivisor (FDiv(e1,e3)) @ simplifyDivisor (FDiv(e2,e3))
  | FDiv(FAdd(e1,e2),e3) -> simplifyDivisor (FAdd(FDiv(e1,e3),FDiv(e1,e3))) 
  | FDiv(e1, e2) ->  simplify e1 |> List.fold (fun s x -> s @ (simplify e2)) [[]]
  | FNum c          -> [[ANum 1.0]]
  | FVar s          -> [[ANum 1.0]]
  | FAdd(e1,e2)     -> simplifyDivisor e1 @ simplifyDivisor e2
  | FMult(e1,e2)    -> combine (simplifyDivisor e1) (simplifyDivisor e2)
  | FExponent(FDiv(e1,e2), n) as f -> simplify f |> List.fold (fun s x -> s @ (simplify e2)) [[]]
  | FExponent(e1,0) -> [[ANum 1.0]]
  | FExponent(e1,1) -> simplifyDivisor e1
  | FExponent(e1,n) when n < 0 -> simplifyDivisor (FDiv(FNum 1.0, FExponent(e1,System.Math.Abs(n))))
  | FExponent(e1,n) -> simplifyDivisor (FMult(e1, FExponent(e1, n-1)))
  | FRoot(_,_) -> failwith "Not implemented"





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
let simplifySimpleExpr (SE (ags1, ags2)) =
  //simplify each atom group
  let ags1' = List.map simplifyAtomGroup ags1
  //simplify divisor 
  
  let ags2' = List.map simplifyAtomGroup ags2

  let ags2' = List.filter (fun x -> not (List.isEmpty x)) ags2'
  // Add atom groups with only constants together.
  let cFolder (a, s, d) elem divisor =
        match (a, elem) with
        | ([ANum (n)], [ANum (f)]) -> match divisor with
                                       |[ANum (1.0)] -> ([ANum (f+n)], s, d) //acc numbers
                                       | _ -> (a, elem :: s, divisor::d)
        | _ -> (a, elem :: s, divisor::d) //just cons rest of ag'

  let (agConst, ags1'', ags2'') = List.fold2 cFolder ([ANum (0.0)], [], []) ags1' ags2'
  
  let pp = ppSimpleExpr (SE(ags1'',ags2''))
  // Last task is to group similar atomGroups into one group
  let eFolder map elem div =  //map ag to their number of appearance

        let count = if Map.containsKey (elem,div) map //update if exists
                    then (Map.find (elem,div) map) + 1.0 else 1.0 
        if List.isEmpty elem 
        then map
        else Map.add (elem,div) count map
  let agSimMap = List.fold2 eFolder Map.empty ags1'' ags2''
  let filterAg ((s,(d:atom list)), f) = if f <> 1.0 && f <> 0.0 
                                        then ((ANum (f)) :: s, d) else (s,d)
  let agSim = (Map.toList agSimMap)
  let (agS, agD) = agSim |> List.map filterAg |> List.unzip
  if agConst.Head = ANum (0.0) then SE (agS, agD) //dispose 0s
  else SE (agConst :: agS, [ANum 1.0] :: agD) 

let exprToSimpleExpr e = simplifySimpleExpr (SE ((simplify e), (simplifyDivisor e)))

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
let splitAG v m s d = 
 match s,d with
  | ([],[]) -> m
  | (ag,agd) ->
        let eqV = function AExponent(v',_) -> v = v' | _ -> false
        let addMap d ag agd m = 
            let agTrimmed = List.filter (removeV v) ag
            let agdTrimmed = List.filter (removeV v) agd
            if Map.containsKey d m
            then match Map.find d m with
                 | SE (agList, agDList) -> Map.add d (SE (agTrimmed :: agList, agdTrimmed::agDList)) m
            else Map.add d (SE ([agTrimmed], [agdTrimmed])) m
        match List.tryFind eqV ag with
          | Some (AExponent(_,d)) ->
            let ag' = List.filter (not << eqV) ag
            let agd' = List.filter (not << eqV) agd
            addMap d ag' agd' m
          | Some _ -> failwith "splitAG: Must never come here! - ANum will not match eqV"
          | None -> addMap 0 ag agd m

let simpleExprToPoly (SE (ags, agd)) (v:string) =
  let k = ppSimpleExpr (SE(ags, agd))
  Po (List.fold2 (splitAG v) Map.empty ags agd)

let exprToPoly e v = (exprToSimpleExpr >> simplifySimpleExpr >> simpleExprToPoly) e v
