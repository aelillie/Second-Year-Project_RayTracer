module Transformation

type shape = unit

type Transformation =
    | T of float[,] * float[,]

let idMatrix = Array2D.init<float> 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)
    
let scale x y z = let m = Array2D.init<float> 4 4 
                           (fun row col -> match row = col with
                                           | true -> match row with
                                                     | 0 -> x
                                                     | 1 -> y
                                                     | 2 -> z
                                                     | 3 -> 1.0
                                                     | _ -> failwith "Out of bounds"
                                           | false -> 0.0)
                  T(m, m)