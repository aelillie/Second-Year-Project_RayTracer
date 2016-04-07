module Transformation

type shape = unit

type Transformation =
    | T of float[,] * float[,]

let idMatrix = Array2D.init<float> 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)
    
let makeInverse (m : float[,]) =
    let a = m.[0, 3]*(-1.0)
    let b = m.[1, 3]*(-1.0)
    let c = m.[2, 3]*(-1.0)
    Array2D.set m 0 3 a
    Array2D.set m 1 3 b
    Array2D.set m 2 3 c
    m

let scale x y z = 
    let m = Array2D.zeroCreate<float> 4 4
    Array2D.set m 0 0 x
    Array2D.set m 1 1 y
    Array2D.set m 2 2 z
    Array2D.set m 3 3 1.0
    T(m, makeInverse m)