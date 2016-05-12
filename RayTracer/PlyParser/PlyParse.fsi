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
