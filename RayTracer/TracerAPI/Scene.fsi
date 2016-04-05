module Scene

type Camera = Camera.Camera
type Sphere = Sphere.Sphere

val mkScene : shapes : shape list -> lights : light list -> ambientLight -> camera -> max_reflect : int -> scene
