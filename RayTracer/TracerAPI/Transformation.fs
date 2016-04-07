module Transformation

type shape = unit

type Transformation =
    | T of float[,] * float[,]

let idMatrix = Array2D.init<float> 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)
    
let makeInverse a =
    let m = Array2D.copy a //Arrays are mutable
    m.[0, 3] <- m.[0, 3]*(-1.0)
    m.[1, 3] <- m.[1, 3]*(-1.0)
    m.[2, 3] <- m.[2, 3]*(-1.0)
    m

let scale x y z = 
    let m = idMatrix
    m.[0, 0] <- x
    m.[1, 1] <- y
    m.[2, 2] <- z
    T(m, makeInverse m)