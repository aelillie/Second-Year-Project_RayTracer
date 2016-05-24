module PlyParse

type Ply = 
         | Vertex of float []
         | Property of string
         | Face of int []
         | Comment of string
         | DummyData of string
         | Element of string * int * Ply list
         | Endheader 

// Takes a filepath as string to a ply file and returns a list of type Ply generated from the ply file.
val parsePly : string -> Ply list
//val print : Ply -> TextWriter -> unit

val textureIndexes : _arg1:Ply list -> (int * int) option
val XYZIndexes : _arg1:Ply list -> (int * int * int) option //TODO: Should be generic function
val normIndexes : _arg1: Ply list -> (int * int * int) option
val faceCount : _arg1:Ply list -> int
val vertices : p:Ply list -> float [] array
val faces : p:Ply list -> int [] array
