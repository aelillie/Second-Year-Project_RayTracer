module PlyParse
open System.IO
type Ply

// Takes a filepath as string to a ply file and returns a list of type Ply generated from the ply file.
val parsePly : string -> Ply list
val print : Ply -> TextWriter -> unit
