module PlyParse

type Ply = 
         | Vertex of float * float * float
         | Face of int list
         | Comment of string
         | DummyData
         | Element of string * int
         | Endheader 

val parsePly : string -> Ply list

