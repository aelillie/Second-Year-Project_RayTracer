module Scene

open Camera
open Shape
open Light
open Tracer
open Ray
open Point
open Vector
type Scene

val mkScene : shapes : Shape list -> lights : Light list -> AmbientLight -> Camera -> max_reflect : int -> Scene

val renderToFile : Scene -> string -> unit