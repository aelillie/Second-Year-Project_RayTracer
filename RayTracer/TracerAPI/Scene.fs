module Scene

type Camera = Camera.Camera

type Scene =
  | S of shape list * light list * ambientLight * camera * int

let mkScene shapes lights ambientLight camera reflection = S(shapes, lights, ambientLight, camera, reflection)
