module Transformation
open System.Threading.Tasks
open Point
open Vector

type shape = unit //Dummy type for testing

type Transformation =
    | T of float[,] * float[,] //normal matrix and inverse matrix
    override t.ToString() =
        match t with
            T(m, m') -> "[["+m.[0,0].ToString()+" ; "+m.[0,1].ToString()+" ; "+m.[0,2].ToString()+" ; "+m.[0,3].ToString()+"]\n"+
                        " ["+m.[1,0].ToString()+" ; "+m.[1,1].ToString()+" ; "+m.[1,2].ToString()+" ; "+m.[1,3].ToString()+"]\n"+
                        " ["+m.[2,0].ToString()+" ; "+m.[2,1].ToString()+" ; "+m.[2,2].ToString()+" ; "+m.[2,3].ToString()+"]\n"+
                        " ["+m.[3,0].ToString()+" ; "+m.[3,1].ToString()+" ; "+m.[3,2].ToString()+" ; "+m.[3,3].ToString()+"]]\n"
                        + ",\n" +
                        "[["+m'.[0,0].ToString()+" ; "+m'.[0,1].ToString()+" ; "+m'.[0,2].ToString()+" ; "+m'.[0,3].ToString()+"]\n"+
                        " ["+m'.[1,0].ToString()+" ; "+m'.[1,1].ToString()+" ; "+m'.[1,2].ToString()+" ; "+m'.[1,3].ToString()+"]\n"+
                        " ["+m'.[2,0].ToString()+" ; "+m'.[2,1].ToString()+" ; "+m'.[2,2].ToString()+" ; "+m'.[2,3].ToString()+"]\n"+
                        " ["+m'.[3,0].ToString()+" ; "+m'.[3,1].ToString()+" ; "+m'.[3,2].ToString()+" ; "+m'.[3,3].ToString()+"]]\n"

let mkTMatrix (m : float[,]) (m' : float[,]) = T(m, m')
let getT (T(m, m')) = m
let getInv (T(m, m')) = m'

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

let mirrorX =
    let m = array2D [[-1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    T(m, m) //Its own inverse transformation matrix

let mirrorY =
    let m = array2D [[1.0;  0.0; 0.0; 0.0]
                     [0.0; -1.0; 0.0; 0.0]
                     [0.0;  0.0; 1.0; 0.0]
                     [0.0;  0.0; 0.0; 1.0]]
    T(m, m)

let mirrorZ =
    let m = array2D [[1.0; 0.0;  0.0; 0.0]
                     [0.0; 1.0;  0.0; 0.0]
                     [0.0; 0.0; -1.0; 0.0]
                     [0.0; 0.0;  0.0; 1.0]]
    T(m, m)

let degrees_to_radians (d : float) = d * System.Math.PI / 180.0
 
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

let rotateX1 angle = rotateX (degrees_to_radians angle)


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

let rotateY1 angle = rotateX (degrees_to_radians angle)

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

let rotateZ1 angle = rotateX (degrees_to_radians angle)

let inverseShear (m : float [,]) = 
    let m' = Array2D.copy m
    let d = 1.0 - m.[1,0]*m.[0,1]-m.[2,0]*m.[0,2]-m.[2,1]*m.[1,2]
                + m.[1,0]*m.[2,1]*m.[0,2]+m.[2,0]*m.[0,1]*m.[1,2] 
    m'.[0,0] <-  1.0-m.[2,1]*m.[1,2]
    m'.[1,0] <- -m.[1,0]+m.[2,0]*m.[1,2]
    m'.[2,0] <- -m.[2,0]+m.[1,0]*m.[2,1]
    m'.[0,1] <- -m.[0,1]+m.[2,1]*m.[0,2]
    m'.[1,1] <-  1.0-m.[2,0]*m.[0,2]
    m'.[2,1] <- -m.[2,1]+m.[2,0]*m.[0,1]
    m'.[0,2] <- -m.[0,2]+m.[0,1]*m.[1,2]
    m'.[1,2] <- -m.[1,2]+m.[1,0]*m.[0,2]
    m'.[2,2] <-  1.0-m.[1,0]*m.[0,1]
    m'.[3,3] <- d
    Array2D.map (fun elem -> elem/d) m'

exception SheareWithZero

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

let sheareXZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ distance; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

let sheareYX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; distance; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

let sheareYZ distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; distance; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

let sheareZX distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; distance; 0.0]
                     [ 0.0; 1.0; 0.0; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')

let sheareZY distance = 
    if distance = 0.0 then raise SheareWithZero
    let m = array2D [[ 1.0; 0.0; 0.0; 0.0]
                     [ 0.0; 1.0; distance; 0.0]
                     [ 0.0; 0.0; 1.0; 0.0]
                     [ 0.0; 0.0; 0.0; 1.0]]
    let m' = inverseShear m
    T(m, m')
        

let transpose (m : float[,]) = 
    Array2D.init<float> 4 4 (fun row col -> m.[col, row])

//From https://msdn.microsoft.com/en-us/library/hh304369(v=vs.100).aspx
let matrixMult (a:float[,]) (b:float[,]) =
    let rowsA, colsA = Array2D.length1 a, Array2D.length2 a
    let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
    let result = Array2D.create rowsA colsB 0.0
    Parallel.For(0, rowsA, (fun i-> //row number
        for j = 0 to colsB - 1 do
           for k = 0 to colsA - 1 do
              result.[i,j] <- result.[i,j] + a.[i,k] * b.[k,j]))  
    |> ignore
    result

let transPoint (m : float[,]) (p : Point) =
    let px = [|Point.getX p; Point.getY p; Point.getZ p; 1.0|]
    let out = [|0.0; 0.0; 0.0; 0.0;|]
    Array2D.iteri (fun row col elem -> out.[row] <- out.[row] + px.[col] * elem) m
    Point.mkPoint out.[0] out.[1] out.[2]

let transPointParallel (m : float[,]) (p : Point) =
    let px = array2D [[Point.getX p]
                      [Point.getY p]
                      [Point.getZ p]
                      [1.0]]
    let out = matrixMult m px
    Point.mkPoint out.[0,0] out.[1,0] out.[2,0]

//Same as transPoint, except the vector has a 0.0 in its
//4th dimension
let transVector (m : float[,]) (v : Vector) =
    let vx = [|Vector.getX v; Vector.getY v; Vector.getZ v; 0.0|]
    let out = [|0.0; 0.0; 0.0; 0.0;|]
    Array2D.iteri (fun row col elem -> out.[row] <- out.[row] + vx.[col] * elem) m
    Vector.mkVector out.[0] out.[1] out.[2]

let transVectorParallel (m : float[,]) (v : Vector) =
    let vx = array2D [[Vector.getX v]
                      [Vector.getY v]
                      [Vector.getZ v]
                      [0.0]]
    let out = matrixMult m vx
    Vector.mkVector out.[0,0] out.[1,0] out.[2,0]

let mergeTransformations (tL : Transformation list) =
    let idM = Array2D.init<float> 4 4 (fun row col -> if row = col then 1.0 else 0.0)
    let m = List.foldBack (fun elem acc -> matrixMult acc (getT elem)) tL idM
    let m' = List.fold (fun acc elem -> matrixMult acc (getInv elem)) idM tL
    mkTMatrix m m'

