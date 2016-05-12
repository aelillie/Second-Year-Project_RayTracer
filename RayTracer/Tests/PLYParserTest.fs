namespace TestSuite

open System
open Shapes.Shape
open Texture
open PlyParse

module PLYParserTest =

    let chk (name,res1,res2) =
      printf "%s %s\n" name (if res1 = res2 then "OK" else "FAILED\nnExpected:\n"+(string)res1+"\nActual:\n"+(string)res2+"\n")
    
    let ply = parsePly "../../../ply/textureCoordTest.ply"
    let (u, v) = textureIndexes ply
    
    let tests =
        ["Test for u, v index", "6, 7", (string u) + ", " + (string v)
         ] 
    
    
    let doTest() =
      printf "PLYParserTest\n"
      List.iter chk tests
    
    
    