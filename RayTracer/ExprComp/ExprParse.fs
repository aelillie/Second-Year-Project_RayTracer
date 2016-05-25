module ExprParse

(* Grammar:

E    = T Eopt .
Eopt = "+" T Eopt | e .
T    = F Topt .
Topt = "*" F Topt | e .
F    = P Fopt .
Fopt = "^" Int | e .
P    = Int [ Float | Var | "(" E ")" .

e is the empty sequence.
*)

type terminal =
  Add | Mul | Pwr | Lpar | Rpar | Int of int | Float of float | Var of string | Root | Div | Neg

let isblank c = System.Char.IsWhiteSpace c
let isdigit c  = System.Char.IsDigit c
let isletter c = System.Char.IsLetter c
let isletterdigit c = System.Char.IsLetterOrDigit c

let explode s = [for c in s -> c]

let floatval (c:char) = float((int)c - (int)'0')
let intval(c:char) = (int)c - (int)'0'

exception Scanerror

let rec scnum (cs, value, neg) = 
  match cs with 
    '.' :: c :: cr when isdigit c -> scfrac(c :: cr, (float)value, 0.1, neg)
  | c :: cr when isdigit c -> scnum(cr, 10* value + intval c,neg)
  | _ -> if neg
         then 
           let value = value*(-1)
           (cs,Int value ) 
         else
           (cs, Int value)   (* Number without fraction is an integer. *)
and scfrac (cs, value, wt, neg) =
  match cs with
    c :: cr when isdigit c -> scfrac(cr, value+wt*floatval c, wt/10.0, neg)
  | _ -> if neg
         then
           let value = value * -1.0
           (cs, Float value)
         else
           (cs, Float value)

let rec scname (cs, value) =
  match cs with
    c :: cr when isletterdigit c -> scname(cr, value + c.ToString())
  | _ -> (cs, value)

let scan s =
  let rec sc cs = 
    match cs with
      [] -> []
    | '+' :: cr -> Add :: sc cr      
    | '*' :: cr -> Mul :: sc cr      
    | '^' :: cr -> Pwr :: sc cr
    | '(' :: cr -> Lpar :: sc cr     
    | ')' :: cr -> Rpar :: sc cr
    | '/' :: cr -> Div :: sc cr
    | '_' :: cr -> Root :: sc cr     
    | '-' :: c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c, true)
                                       t :: sc cs1
    | '-' :: b :: cr when isblank b -> sc ('+'::'-'::cr)
    | '-' :: cr -> sc ('-'::'1'::'*'::cr)

    | c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c, false) 
                                t :: sc cs1
    | c :: cr when isblank c -> sc cr
    | c :: cr when isletter c -> let (cs1, n) = scname(cr, (string)c)
                                 Var n :: sc cs1
    | _ -> raise Scanerror
  sc (explode s)

let rec insertMult = function
  Float r :: Var x :: ts     -> Float r::Mul::insertMult (Var x::ts)
| Float r1 :: Float r2 :: ts -> Float r1::Mul::insertMult (Float r2::ts)
| Float r :: Int i :: ts     -> Float r::Mul::insertMult(Int i::ts)
| Var x :: Float r :: ts     -> Var x::Mul::insertMult (Float r::ts)
| Var x1 :: Var x2 :: ts     -> Var x1::Mul::insertMult (Var x2::ts)
| Var x :: Int i :: ts       -> Var x::Mul::insertMult (Int i::ts)
| Int i :: Float r :: ts     -> Int i::Mul::insertMult (Float r::ts)
| Int i :: Var x :: ts       -> Int i::Mul::insertMult (Var x::ts)
| Int i1 :: Int i2 :: ts     -> Int i1::Mul::insertMult (Int i2::ts)
| Float r :: Lpar :: ts      -> Float r::Mul::Lpar::insertMult ts
| Var x :: Lpar :: ts        -> Var x::Mul::Lpar::insertMult ts
| Int i :: Lpar :: ts        -> Int i::Mul::Lpar::insertMult ts
| Var x :: Root :: ts        -> Var x::Mul::insertMult (Root::ts)
| Int i :: Root :: ts        -> Int i::Mul::insertMult (Root::ts)
| t :: ts                    -> t :: insertMult ts
| []                         -> []

(* Grammar:

E    = T Eopt .
Eopt = "+" T Eopt | e .
T    = F Topt .
Topt = "*" F Topt | e .
F    = P Fopt .
Fopt = "^" Int | e .
P    = Int | Float | Var | "(" E ")" .

e is the empty sequence.
*)
  
type expr = 
  | FNum of float
  | FVar of string
  | FAdd of expr * expr
  | FMult of expr * expr
  | FExponent of expr * int
  | FRoot of expr*int
  | FDiv of expr * expr

exception Parseerror

let rec E (ts:terminal list) = (T >> Eopt) ts
and Eopt (ts, inval) =
  match ts with
    | Add :: tr -> let (ts1, tv) = T tr
                   Eopt (ts1, FAdd(inval, tv))
    | _ -> (ts, inval)
and T ts = (F >> Topt) ts
and Topt (ts, inval) =
    match ts with
    | Mul :: tr -> let (ts1, fv) = F tr
                   Topt (ts1, FMult(inval, fv))
    | Div :: tr -> let (ts1, fv) = F tr
                   Topt (ts1, FDiv(inval, fv))
    | _ -> (ts, inval)
and F ts = (P >> Fopt) ts
and Fopt (ts, inval) =
    match ts with
    | Pwr :: Int i :: tr -> (tr, FExponent(inval, i))
    | Root :: Int i :: tr -> (tr,FRoot(inval,i))
    | _ -> (ts, inval)
and P ts =
    match ts with
    | Int i :: tr   -> (tr, FNum(float i))
    | Float f :: tr -> (tr, FNum(f))
    | Var v :: tr   -> (tr, FVar(v))
    | Lpar :: tr    -> let (ts1, ev) = E tr
                       match ts1 with
                       | Rpar :: tr -> (tr, ev)
                       | _ -> raise Parseerror
    | _             -> raise Parseerror

let parse ts = 
  let r = E ts
  match E ts with
    ([], result) -> result
  | _ -> raise Parseerror

let parseStr s = (scan >> insertMult >> parse) s

let dotAST ast =
  let fixStr (s:string) = s.Replace ("\"", "\\\"")
  let genDot s n e = "digraph G {\nlabel=\"" + (fixStr s) + "\"\n" + n + e + "\n}"
  // i is unique label such that nodes and edges are unique in DiGraph.
  let genNodeStr i l = "Node"+(string i)+" [label=\""+l+"\"];\n"
  let genEdgeStr i1 i2 = "Node"+(string i1)+" -> " + "Node"+(string i2)+";\n"
  // Edges are unique and stored in a set.
  // Nodes are not unique and stored in a map, i.e., node with "+" may happen several times. 
  let rec genNE (i,nmap,eset) = function
    FNum r -> (i,Map.add i (genNodeStr i ((string)r)) nmap,eset)            // Add node with number
  | FVar x -> (i,Map.add i (genNodeStr i x) nmap,eset)                      // Add node with variable
  | FAdd (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1         // Generate nodes and edges for e1 and e2
                    let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                    (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "+") nmap2,                      // Add node for "+"
                     Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "+"->e1 and "+"->e2
  | FMult (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1        // Generate nodes and edges for e1 and e2
                     let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                     (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "*") nmap2,                      // Add node for "*"
                      Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "*"->e1 and "*"->e2
  | FExponent (e1,ie) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1                                // Generate nodes and edges for e1
                         let (i2,nmap2) = (i1+1,Map.add (i1+1) (genNodeStr (i1+1) ((string)ie)) nmap1)  // Add node for integer (exponent)
                         (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "^") nmap2,                            // Add node for "^"
                          Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset1))        // Add edges for "^"->e1 and "^"->ie
  let (_,nmap,eset) = genNE (0,Map.empty,Set.empty) ast  // Generate map for nodes and set for edges
  genDot (sprintf "%A\n" ast) (Map.fold (fun acc _ s -> acc + s) "" nmap) (Set.fold (fun acc s -> acc + s) "" eset)  // Generate big string with dot-code.


//
//type terminal =
//  Add | Mul | Pwr  | Lpar | Rpar | Int of int | Float of float | Var of string | Root | Div | Neg
// 
//let isblank c = System.Char.IsWhiteSpace c
//let isdigit c  = System.Char.IsDigit c
//let isletter c = System.Char.IsLetter c
//let isletterdigit c = System.Char.IsLetterOrDigit c
// 
//let explode s = [for c in s -> c]
// 
//let floatval (c:char) = float((int)c - (int)'0')
//let intval(c:char) = (int)c - (int)'0'
// 
//exception Scanerror
// 
//let rec scnum (cs, value) =
//  match cs with
//    '.' :: c :: cr when isdigit c -> scfrac(c :: cr, (float)value, 0.1)
//  | c :: cr when isdigit c -> scnum(cr, 10* value + intval c)
//  | _ -> (cs,Int value)    // Number without fraction is an integer.
//and scfrac (cs, value, wt) =
//  match cs with
//    c :: cr when isdigit c -> scfrac(cr, value+wt*floatval c, wt/10.0)
//  | _ -> (cs, Float value)
// 
//let rec scname (cs, value) =
//  match cs with
//    c :: cr when isletterdigit c -> scname(cr, value + c.ToString())
//  | _ -> (cs, value)
// 
//let scan s =
//  let rec sc cs =
//    match cs with
//      [] -> []
//    | '+' :: cr -> Add :: sc cr      
//    | '*' :: cr -> Mul :: sc cr      
//    | '/' :: cr -> Div :: sc cr  
//    | '^' :: cr -> Pwr :: sc cr
//    | '(' :: cr -> Lpar :: sc cr    
//    | ')' :: cr -> Rpar :: sc cr    
//    | '_' :: cr -> Root :: sc cr     
//    | '-' :: c :: cr when isdigit c -> let (cs1, t) = scnum(cr, -1 * intval c)
//                                       Add :: t :: sc cs1
//    | c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c)
//                                t :: sc cs1
//    | c :: cr when isblank c -> sc cr
//    | c :: cr when isletter c -> let (cs1, n) = scname(cr, (string)c)
//                                 Var n :: sc cs1
//    | _ -> raise Scanerror
//  sc (explode s)
// 
//let rec insertMult = function
//  Float r :: Var x :: ts -> insertMult(Float r::Mul::Var x::ts) // This approach is na�ve and might be quite slow for large functions with a lot of implicit multiplications.
//                                                                // Not quite sure how to improve it though.
//| Float r1 :: Float r2 :: ts -> insertMult(Float r1::Mul::Float r2::ts)
//| Float r :: Int i :: ts -> insertMult(Float r::Mul::Int i::ts)
//| Var x :: Float r :: ts -> insertMult(Var x::Mul::Float r::ts)
//| Var x1 :: Var x2 :: ts -> insertMult(Var x1::Mul::Var x2::ts)
//| Var x :: Int i :: ts -> insertMult(Var x::Mul::Int i::ts)
//| Int i :: Float r :: ts -> insertMult(Int i::Mul::Float r::ts)
//| Int i :: Var x :: ts -> insertMult(Int i::Mul::Var x::ts)
//| Int i1 :: Int i2 :: ts -> insertMult(Int i1::Mul::Int i2::ts)
//| Float r :: Lpar :: ts -> insertMult(Float r::Mul::Lpar::ts)
//| Var x :: Lpar :: ts -> insertMult(Var x::Mul::Lpar::ts)
//| Int i :: Lpar :: ts -> insertMult(Int i::Mul::Lpar::ts)
//| t :: ts -> t :: insertMult ts
//| [] -> []
// 
//type expr =
//  | FNum of float
//  | FVar of string
//  | FAdd of expr * expr
//  | FMult of expr * expr
//  | FExponent of expr * int
//  | FRoot of expr*int
//  | FDiv of expr * expr
// 
//exception Parseerror
// 
//let rec E (ts:terminal list) = (T >> Eopt) ts
// 
//and Eopt (ts, inval) =
//  match ts with
//    Add :: tr -> let (rest, expr) = T tr
//                 Eopt (rest, FAdd(inval, expr))
//    | _       -> (ts, inval) // "empty set"
// 
//and T ts = (F >> Topt) ts
// 
//and Topt (ts, inval) =
//    match ts with
//    | Mul :: tr -> let (rest, expr) = F tr
//                   Topt (rest, FMult(inval, expr))
//    | Div :: tr -> let (rest, expr) = F tr
//                   Topt(rest, FDiv(inval,expr))
//    | _         -> (ts, inval) // "empty set"
// 
//and F ts = (P >> Fopt) ts
// 
//and Fopt (ts, inval) =
//    match ts with // When defining powers of, we must have the expression that is to be put to the power, as well as the exponent (integer) to which the expression is to the power of.
//        | Pwr :: Int(y) :: tr -> (tr, FExponent(inval, y))
//        | Root :: Int i :: tr -> (tr,FRoot(inval,i))
//        | _       -> (ts, inval) //  "empty set"
// 
//and P ts =
//    match ts with
//    | Float(x)::tr  -> (tr, FNum(x))
//    | Int(x)::tr    -> (tr, FNum(float x))
//    | Var(x)::tr    -> (tr, FVar(x))
//    | Lpar::tr      -> let (remainTs, innerExpr) = E tr // A parenthesis consists of the left parenthesis, whatever is inside it, and the right parenthesis
//                       match remainTs with // Once the terminals have been evaluated, the next thing in the list of terminals should be a right parenthesis.
//                           | Rpar :: trr -> (trr, innerExpr) // Since recursively calling E will build a tree of expressions, we can safely just return the remaining terminals and the found expression.
//                           | _          -> raise Parseerror // If there is no parenthesis left in the terminal list, the grammar is not fulfilled.
//    | _             -> raise Parseerror // ts doesn't fulfill grammar
// 
// 
//let parse ts =
//  match E ts with
//    ([], expr) -> expr
//  | _  -> raise Parseerror
// 
//let parseStr str =
//    parse(insertMult(scan str))
// 
// 
//let dotAST ast =
//  let fixStr (s:string) = s.Replace ("\"", "\\\"")
//  let genDot s n e = "digraph G {\nlabel=\"" + (fixStr s) + "\"\n" + n + e + "\n}"
//  // i is unique label such that nodes and edges are unique in DiGraph.
//  let genNodeStr i l = "Node"+(string i)+" [label=\""+l+"\"];\n"
//  let genEdgeStr i1 i2 = "Node"+(string i1)+" -> " + "Node"+(string i2)+";\n"
//  // Edges are unique and stored in a set.
//  // Nodes are not unique and stored in a map, i.e., node with "+" may happen several times.
//  let rec genNE (i,nmap,eset) = function
//    FNum r -> (i,Map.add i (genNodeStr i ((string)r)) nmap,eset)            // Add node with number
//  | FVar x -> (i,Map.add i (genNodeStr i x) nmap,eset)                      // Add node with variable
//  | FAdd (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1         // Generate nodes and edges for e1 and e2
//                    let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
//                    (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "+") nmap2,                      // Add node for "+"
//                     Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "+"->e1 and "+"->e2
//  | FMult (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1        // Generate nodes and edges for e1 and e2
//                     let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
//                     (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "*") nmap2,                      // Add node for "*"
//                      Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "*"->e1 and "*"->e2
//  | FExponent (e1,ie) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1                                // Generate nodes and edges for e1
//                         let (i2,nmap2) = (i1+1,Map.add (i1+1) (genNodeStr (i1+1) ((string)ie)) nmap1)  // Add node for integer (exponent)
//                         (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "^") nmap2,                            // Add node for "^"
//                          Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset1))        // Add edges for "^"->e1 and "^"->ie
//  let (_,nmap,eset) = genNE (0,Map.empty,Set.empty) ast  // Generate map for nodes and set for edges
//  genDot (sprintf "%A\n" ast) (Map.fold (fun acc _ s -> acc + s) "" nmap) (Set.fold (fun acc s -> acc + s) "" eset)  // Generate big string with dot-code.
