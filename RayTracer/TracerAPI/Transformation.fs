module Transformation

type shape = unit

type Transformation =
    | T of float[,] * float[,]

let idMatrix = Array2D.init<float> 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)
    