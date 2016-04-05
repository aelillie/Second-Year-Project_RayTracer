module Scene

type camera = Camera.Camera
type shape = Dummy
type light = Dummy
type ambientLight = Dummy

type Scene

val mkScene : shapes : shape list -> lights : light list -> ambientLight -> camera -> max_reflect : int -> Scene
