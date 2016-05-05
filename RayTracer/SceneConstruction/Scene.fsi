module Scene

open Camera
open Light
open Ray
open Point
open Vector
type Scene
type Shape = Shapes.BasicShape.Shape

//Creates a scene containing all the elements.
val mkScene : shapes : Shape list -> lights : Light list -> AmbientLight -> Camera -> max_reflect : int -> Scene

//Takes a scene and renders it to a file using the string value given.
val renderToFile : Scene -> string -> unit

val renderToScreen : Scene -> unit