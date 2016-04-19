module PlyParse

type Ply = 
         | Vertex of float * float * float
         | Face of int list
         | Comment of string
         | DummyData
         | Element of string * int
         | Endheader 

// Takes a filepath as string to a ply file and returns a list of type Ply generated from the ply file.
val parsePly : string -> Ply list
val print : Ply -> TextWriter -> unit
