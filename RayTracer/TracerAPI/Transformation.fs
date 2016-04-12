///Transformation matrices for object manipulation
module Transformation
open Point

type shape = unit //Dummy type for testing

type Transformation =
    | T of float[,] * float[,] //normal matrix and inverse matrix

let getTransMatrix (T(m, m')) = m
let getInvTransMatrix (T(m, m')) = m'

///'Base' matrix for all transformations
let idMatrix = Array2D.init 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)

///Move a shape in some direction/dimension
let translate x y z =
    let m = idMatrix
    m.[0, 3] <- x
    m.[1, 3] <- y
    m.[2, 3] <- z
    let m' = Array2D.copy m //Use for inverse matrix
    m'.[0, 3] <- -1.0*x
    m'.[1, 3] <- -1.0*y
    m'.[2, 3] <- -1.0*z
    T(m, m')

///Scale a shape with a factor n in some direction/dimension
let scale x y z = 
    let m = idMatrix
    m.[0, 0] <- x
    m.[1, 1] <- y
    m.[2, 2] <- z
    let m' = Array2D.copy m
    m'.[0, 0] <- 1.0/x
    m'.[1, 1] <- 1.0/y
    m'.[2, 2] <- 1.0/z
    T(m, m')

///Mirror shape around the x-axis
let mirrorX =
    let m = idMatrix
    m.[0,0] <- -1.0
    T(m, m) //Its own inverse transformation matrix

///Mirror shape around the y-axis
let mirrorY =
    let m = idMatrix
    m.[1,1] <- -1.0
    T(m, m)

///Mirror shape around the z-axis
let mirrorZ =
    let m = idMatrix
    m.[2,2] <- -1.0
    T(m, m)
 
///Rotate shape around the x-axis by the given angle
///measured in radians
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

///Rotate shape around the y-axis by the given angle
///measured in radians
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

///Rotate shape around the z-axis by the given angle
///measured in radians
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

exception SheareWithZero

///Shear shape some distance with respect
///to the x- and y-axis
let sheareXY distance = 
//Just to notify the user. The id matrix will
//have a 0.0 in its place either way.
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[1, 0] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

///Shear shape some distance with respect
///to the x- and z-axis
let sheareXZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[2, 0] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

///Shear shape some distance with respect
///to the y- and x-axis
let sheareYX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[0, 1] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

///Shear shape some distance with respect
///to the y- and z-axis
let sheareYZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[2, 1] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

///Shear shape some distance with respect
///to the z- and x-axis
let sheareZX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[0, 2] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

///Shear shape some distance with respect
///to the z- and y-axis
let sheareZY distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = idMatrix
    m.[1, 2] <- distance
    let m' = failwith "Missing inverse matrix"
    T(m, m')

///Transpose a transformation matrix
///by swapping all rows with columns
let transpose (m : float[,]) = 
    Array2D.init<float> 4 4 (fun row col -> m.[col, row])

///Multiply a transformation matrix with some point.
///Returns a new point, with same dimensions, but
///with updated values
let multTrans (t : Transformation) (p : Point) =
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