module Scene

type Camera = Camera.Camera

type Scene

val mkScene : shapes : shape list -> lights : light list -> ambientLight -> camera -> max_reflect : int -> scene
