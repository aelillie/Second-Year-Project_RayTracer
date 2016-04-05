module Scene

type camera = Camera.Camera
type shape = Dummy
type light = Dummy
type ambientLight = Dummy

type Scene =
  | S of shape list * light list * ambientLight * camera * int

let mkScene shapes lights ambientLight camera reflection = S(shapes, lights, ambientLight, camera, reflection)
