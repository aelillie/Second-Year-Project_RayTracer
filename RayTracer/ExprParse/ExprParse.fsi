module ExprParse

type terminal = Add | Mul | Pwr | Lpar | Rpar | Int of int | Float of float | Var of string
exception Scanerror
val scan: char seq -> terminal list
val insertMult: terminal list -> terminal list

type expr = FNum of float | FVar of string | FAdd of expr*expr | FMult of expr*expr | FExponent of expr*int
exception Parseerror
val parse: terminal list -> expr
val parseStr: seq<char> -> expr
val dotAST: expr -> string