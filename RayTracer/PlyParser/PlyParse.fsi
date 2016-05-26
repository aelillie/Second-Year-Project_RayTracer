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

///Find the indexes for textures in a vertex, if any
val textureIndexes : _arg1:Ply list -> (int * int) option
///Find the indexes for XYZ vertex coordinates (if any)
val XYZIndexes : _arg1:Ply list -> (int * int * int) option
///Find the indexes for a vertex's normal vector, if any
val normIndexes : _arg1: Ply list -> (int * int * int) option
///Total count of faces
val faceCount : _arg1:Ply list -> int
///An array of vertices, each vertex being an array of float coordinates
val vertices : p:Ply list -> float [] array
///An array of faces, each face being an array vertex indexes
val faces : p:Ply list -> int [] array
