module PlyParse

type Ply = 
         | Vertex of float * float * float
         | Vertex2 of float * float * float * float * float
         | Vertex3 of float * float * float * float * float * float * float * float
         | Face of int list
         | Comment of string
         | DummyData of string
         | Element of string * int
         | Endheader 

// Takes a filepath as string to a ply file and returns a list of type Ply generated from the ply file.
val parsePly : string -> Ply list
//val print : Ply -> TextWriter -> unit
