///Transformation matrices for object manipulation
module Transformation
open System.Threading.Tasks
open Point
open Vector

type shape = unit //Dummy type for testing

type Transformation =
    | T of float[,] * float[,] //normal matrix and inverse matrix

let getT (T(m, m')) = m
let getInv (T(m, m')) = m'

///'Base' matrix for all transformations
let idMatrix = Array2D.init 4 4 
                (fun row col -> if row = col then 1.0 else 0.0)

///Move a shape in some direction/dimension
let translate x y z =
    let m = array2D [[1.0; 0.0; 0.0; x]
                     [0.0; 1.0; 0.0; y]
                     [0.0; 0.0; 1.0; z]
                     [0.0; 0.0; 0.0; 1.0]]
    let m' = array2D [[1.0; 0.0; 0.0; -x]
                      [0.0; 1.0; 0.0; -y]
                      [0.0; 0.0; 1.0; -z]
                      [0.0; 0.0; 0.0; 1.0]]
    T(m, m')

///Scale a shape with a factor n in some direction/dimension
let scale x y z = 
    let m = array2D [[x; 0.0; 0.0; 0.0]
                     [0.0; y; 0.0; 0.0]
                     [0.0; 0.0; z; 0.0]
                     [0.0; 0.0; 0.0; 1.0]]
    let m' = array2D [[1.0/x; 0.0; 0.0; 0.0]
                      [0.0; 1.0/y; 0.0; 0.0]
                      [0.0; 0.0; 1.0/z; 0.0]
                      [0.0; 0.0; 0.0; 1.0]]
    T(m, m')

///Mirror shape around the x-axis
let mirrorX =
    let m = array2D [[-1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    T(m, m) //Its own inverse transformation matrix

///Mirror shape around the y-axis
let mirrorY =
    let m = array2D [[1.0;  0.0; 0.0; 0.0]
                     [0.0; -1.0; 0.0; 0.0]
                     [0.0;  0.0; 1.0; 0.0]
                     [0.0;  0.0; 0.0; 1.0]]
    T(m, m)

///Mirror shape around the z-axis
let mirrorZ =
    let m = array2D [[1.0; 0.0;  0.0; 0.0]
                     [0.0; 1.0;  0.0; 0.0]
                     [0.0; 0.0; -1.0; 0.0]
                     [0.0; 0.0;  0.0; 1.0]]
    T(m, m)
 
///Rotate shape around the x-axis by the given angle
///measured in radians
let rotateX angle =
    let cos = System.Math.Cos angle
    let sin = System.Math.Sin angle
    let m = array2D [[1.0; 0.0; 0.0; 0.0]
                     [0.0; cos;-sin; 0.0]
                     [0.0; sin; cos; 0.0]
                     [0.0; 0.0; 0.0; 1.0]]
    let m' = array2D [[1.0; 0.0; 0.0; 0.0]
                      [0.0; cos; sin; 0.0]
                      [0.0;-sin; cos; 0.0]
                      [0.0; 0.0; 0.0; 1.0]]
    T(m, m')

///Rotate shape around the y-axis by the given angle
///measured in radians
let rotateY angle =
    let cos = System.Math.Cos angle
    let sin = System.Math.Sin angle
    let m = array2D [[ cos; 0.0; sin; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [-sin; 0.0; cos; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = array2D [[cos; 0.0;-sin; 0.0]
                      [0.0; 1.0; 0.0; 0.0]
                      [sin; 0.0; cos; 0.0]
                      [0.0; 0.0; 0.0; 1.0]]
    T(m, m')

///Rotate shape around the z-axis by the given angle
///measured in radians
let rotateZ angle =
    let cos = System.Math.Cos angle
    let sin = System.Math.Sin angle
    let m = array2D [[ cos;-sin; 0.0; 0.0]
                     [ sin; cos; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = array2D [[ cos; sin; 0.0; 0.0]
                      [-sin; cos; 0.0; 0.0]
                      [ 0.0; 0.0; 1.0; 0.0]
                      [ 0.0; 0.0; 0.0; 1.0]]
    T(m, m')

///Inverse any shearing transformation matrix
let inverseShear (m : float [,]) = 
    let m' = Array2D.copy m
    let d = 1.0 - m.[1,0]*m.[0,1]-m.[2,0]*m.[0,3]-m.[2,1]*m.[1,3]
                + m.[1,0]*m.[2,1]*m.[0,3]+m.[2,0]*m.[0,1]*m.[1,2] 
    m'.[0,0] <-  1.0-m.[2,1]*m.[1,2]
    m'.[1,0] <- -1.0*m.[1,0]+m.[2,0]*m.[1,2]
    m'.[2,0] <- -1.0*m.[2,0]+m.[1,0]*m.[2,1]
    m'.[0,1] <- -1.0*m.[0,1]+m.[2,1]*m.[0,2]
    m'.[1,1] <-  1.0-m.[0,2]*m.[0,2]
    m'.[2,1] <- -1.0*m.[2,1]+m.[2,0]*m.[0,1]
    m'.[0,2] <- -1.0*m.[0,2]+m.[0,1]*m.[1,2]
    m'.[1,2] <- -1.0*m.[1,2]+m.[1,0]*m.[0,3]
    m'.[2,2] <-  1.0-m.[1,0]*m.[0,1]
    m'.[3,3] <- d
    Array2D.map (fun elem -> elem/d) m'

exception SheareWithZero

///Shear shape some distance with respect
///to the x- and y-axis
let sheareXY distance = 
//Just to notify the user. The id matrix will
//have a 0.0 in its place either way.
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ distance; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

///Shear shape some distance with respect
///to the x- and z-axis
let sheareXZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ distance; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

///Shear shape some distance with respect
///to the y- and x-axis
let sheareYX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; distance; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

///Shear shape some distance with respect
///to the y- and z-axis
let sheareYZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; distance; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

///Shear shape some distance with respect
///to the z- and x-axis
let sheareZX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; distance; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

///Shear shape some distance with respect
///to the z- and y-axis
let sheareZY distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; distance; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')
        

///Transpose a transformation matrix
///by swapping all rows with columns
let transpose (m : float[,]) = 
    Array2D.init<float> 4 4 (fun row col -> m.[col, row])

///Multiply a transformation matrix with some point.
///Returns a new point, with same dimensions, but
///with updated values
let transPoint (m : float[,]) (p : Point) =
    let px = [|Point.getX p; Point.getY p; Point.getZ p; 1.0|]
    let out = [|0.0; 0.0; 0.0; 0.0;|]
    Array2D.iteri (fun row col elem -> out.[row] <- out.[row] + px.[col] * elem) m
    Point.mkPoint out.[0] out.[1] out.[2]

///Multiply a transformation matrix with some vector.
///Returns a new vector, with same dimensions, but
///with updated values
//Same as transPoint, except the vector has a 0.0 in its
//4th dimension
let transVector (m : float[,]) (v : Vector) =
    let vx = [|Vector.getX v; Vector.getY v; Vector.getZ v; 0.0|]
    let out = [|0.0; 0.0; 0.0; 0.0;|]
    Array2D.iteri (fun row col elem -> out.[row] <- out.[row] + vx.[col] * elem) m
    Vector.mkVector out.[0] out.[1] out.[2]

//From https://msdn.microsoft.com/en-us/library/hh304369(v=vs.100).aspx
let matrixMult (a:float[,]) (b:float[,]) =
    let rowsA, colsA = Array2D.length1 a, Array2D.length2 a
    let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
    let result = Array2D.create rowsA colsB 0.0
    Parallel.For(0, rowsA, (fun i->
        for j = 0 to colsB - 1 do
           for k = 0 to colsA - 1 do
              result.[i,j] <- result.[i,j] + a.[i,k] * b.[k,j]))  
    |> ignore
    result

///Merge the given list of transformations into one, such that the resulting
///transformation is equivalent to applying the individual transformations
///from left to right (i.e. starting with the first element in the list).
let mergeTransformations (tL : Transformation list) =
    let idM = Array2D.init<float> 4 4 (fun row col -> if row = col then 1.0 else 0.0)
    let t = T(idM, idM) //initial state of multiplication
    //Apply transformation in reverse order
    List.foldBack (fun (T(a, a')) (T(m, m')) -> // accumulator and element
                    T((matrixMult a m), (matrixMult a' m'))) tL t

