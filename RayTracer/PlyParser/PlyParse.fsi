module PlyParse

type Ply = 
         | Vertex of float list
         | Property of string
         | Face of int list
         | Comment of string
         | DummyData of string
         | Element of string * int * Ply list
         | Endheader 

// Takes a filepath as string to a ply file and returns a list of type Ply generated from the ply file.
val parsePly : string -> Ply list
//val print : Ply -> TextWriter -> unit

val textureIndexes : _arg1:Ply list -> (int * int) option
val XYZIndexes : _arg1:Ply list -> (int * int * int) option //TODO: Should be generic function
val faceCount : _arg1:Ply list -> int
val vertices : p:Ply list -> float list list
val faces : p:Ply list -> int list list
