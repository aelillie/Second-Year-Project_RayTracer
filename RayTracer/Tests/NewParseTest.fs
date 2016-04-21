module NewParseTest

open System.Threading
open System.Globalization
Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture

open ExprParse
open ExprToPoly

let chk (name,t,r) =
  printf "%s %s\n" name (if t = r then "OK" else "FAILED[t="+(string)t+",r="+(string)r+"]")


let tests =
    [("test01","~(3+5*x)",[Sqrt; Lpar; Int 3; Add; Int 5; Mul; Var "x"; Rpar]);]



let doTest() =
  printf "PointTest\n"
  List.iter chk tests

