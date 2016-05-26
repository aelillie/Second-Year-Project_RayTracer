namespace TestSuite

open Transformation

module TransformationTest =

    let chk (name,res1,res2) =
      printf "%s %s\n" name (if res1 = res2 then "OK" else "FAILED\nExpected:\n"+(string)res1+"\nActual:\n"+(string)res2+"\n")
    
    let a = scale 2.0 2.0 2.0      
    let b = rotateX System.Math.PI
    let c = mirrorZ
    let d = sheareYX 1.0
    
    let p = Point.mkPoint 1.0 1.0 1.0
    let pa = transPointParallel (getInv a) p
    let pb = transPointParallel (getInv b) pa
    let pc = transPointParallel (getInv c) pb
    let transP = transPointParallel (getInv d) pc
    
    let norm = mergeTransformations [a;b;c;d]
    let rev = mergeTransformations [d;c;b;a]
    
    let normP = transPointParallel (getInv norm) p
    let revP = transPointParallel (getInv rev) p
    
    let tests =
        [("01 ToString of transformation", (string) norm, (string) norm); 
         ("02 Merge norm with rev", (string) norm, (string) rev);
         ("03 Point trans with rev list", (string) revP, (string) transP);
         ("04 Point trans with norm list", (string) normP, (string) transP)
         ] 
    
    let doTest() =
      printf "TransformationTest\n"
      List.iter chk tests
