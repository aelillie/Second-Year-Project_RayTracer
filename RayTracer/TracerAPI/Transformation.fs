module Transformation
open Point
type shape = unit

type Transformation =
    | T of float[,] * float[,] //normal matrix and inverse matrix

let getTransMatrix (T(m, m')) = m
let getInvTransMatrix (T(m, m')) = m'

let idMatrix = Array2D.init 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)

let scale x y z = 
    let m = idMatrix
    m.[0, 0] <- x
    m.[1, 1] <- y
    m.[2, 2] <- z
    let m' = Array2D.copy m //Arrays are mutable
    m'.[0, 0] <- 1.0/x
    m'.[1, 1] <- 1.0/y
    m'.[2, 2] <- 1.0/z
    T(m, m')

 
let rotateX angle =
    let m = idMatrix
    m.[1, 1] <- System.Math.Cos angle
    m.[2, 2] <- System.Math.Cos angle
    m.[1, 2] <- (System.Math.Sin angle)*(-1.0)
    m.[2, 1] <- System.Math.Sin angle
    let m' = Array2D.copy m
    m'.[1, 2] <- m.[2, 1]
    m'.[2, 1] <- m.[1, 2]
    T(m, m')

let rotateY angle =
    let m = idMatrix
    m.[0, 0] <- System.Math.Cos angle
    m.[2, 2] <- System.Math.Cos angle
    m.[2, 0] <- (System.Math.Sin angle)*(-1.0)
    m.[0, 2] <- System.Math.Sin angle
    let m' = Array2D.copy m
    m'.[2, 0] <- m.[0, 2]
    m'.[0, 2] <- m.[2, 0]
    T(m, m')

let rotateZ angle =
    let m = idMatrix
    m.[0, 0] <- System.Math.Cos angle
    m.[1, 1] <- System.Math.Cos angle
    m.[0, 1] <- (System.Math.Sin angle)*(-1.0)
    m.[1, 0] <- System.Math.Sin angle
    let m' = Array2D.copy m
    m'.[0, 1] <- m.[1, 0]
    m'.[1, 0] <- m.[0, 1]
    T(m, m')
 
let mirrorX =
    let m = idMatrix
    m.[0,0] <- -1.0
    T(m, m)

let mirrorY =
    let m = idMatrix
    m.[1,1] <- -1.0
    T(m, m)

let mirrorZ =
    let m = idMatrix
    m.[2,2] <- -1.0
    T(m, m)

exception SheareWithZero

let sheareXY distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[1, 0] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

let sheareXZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[2, 0] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

let sheareYX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[0, 1] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

let sheareYZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[2, 1] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

let sheareZX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[0, 2] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

let sheareZY distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[1, 2] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

let transpose (m : float[,]) = 
    Array2D.init<float> 4 4 (fun row col -> m.[col, row])

let multiply (t : Transformation) (p : Point) =
    let m = getTransMatrix t
    let mutable x = 0.0
    let mutable y = 0.0
    let mutable z = 0.0
    let mult row col elem =
        match col with
        | 0 -> x <- x + (Point.getX p) * elem
        | 1 -> y <- y + (Point.getY p) * elem
        | 2 -> z <- z + (Point.getZ p) * elem
        | _ -> () //Do nothing
    Array2D.iteri mult m
    Point.mkPoint x y z

    

let mEx =
    let m = idMatrix
    m.[1, 2] <- 5.0
    m.[2, 1] <- 6.4
    m.[3, 0] <- 4.0
    m.[0, 3] <- 2.7
    m