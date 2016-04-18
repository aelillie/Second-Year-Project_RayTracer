module TransformationTest
open Transformation

let chk (name,res1,res2) =
  printf "%s %s\n" name (if res1 = res2 then "OK" else "FAILED\nActual:\n"+(string)res1+"\nExpected:\n"+(string)res2+"]\n")

let a = scale 2.0 2.0 2.0      
let b = rotateX System.Math.PI
let c = mirrorZ
let d = sheareYX 1.0

let res1 = mergeTransformations [a;b;c;d]
let res2 = mergeTransformations [d;c;b;a]
let res3 = mergeTransformations [a;d;b;c]

let tests =
    [("Merge01", (string) res1, (string) res1); 
     ("Merge02", (string) res1, (string) res2)] //Test if order matters

let doTest() =
  printf "TransformationTest\n"
  List.iter chk tests