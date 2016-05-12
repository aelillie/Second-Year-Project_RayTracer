module ExprToPoly

type expr = ExprParse.expr
val subst: expr -> (string * expr) -> expr

type atom = ANum of float | AExponent of string * int| ADiv of atom list list * atom list list
type atomGroup = atom list  
type simpleExpr = SE of atomGroup list
val ppSimpleExpr: simpleExpr -> string
val exprToSimpleExpr: expr -> simpleExpr
val simplifyAtomGroup: atomGroup -> atomGroup
val simplifySimpleExpr: simpleExpr -> simpleExpr
//val Derivative: expr -> expr


type poly =  Po of Map<int,simpleExpr>
val ppPoly: string -> poly -> string
val simpleExprToPoly: simpleExpr -> string -> poly
val exprToPoly: expr -> string -> poly
