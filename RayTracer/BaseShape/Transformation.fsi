module Transformation
open Point
open Vector

type Transformation

val getT : Transformation -> float[,]
val getInv : Transformation -> float[,]
val transpose : float[,] -> float[,]
val transPoint : float[,] -> Point -> Point
val transVector : float[,] -> Vector -> Vector

val translate : x : float -> y : float -> z : float -> Transformation
val rotateX : angle : float -> Transformation
val rotateY : angle : float -> Transformation
val rotateZ : angle : float -> Transformation
val sheareXY : distance : float -> Transformation
val sheareXZ : distance : float -> Transformation
val sheareYX : distance : float -> Transformation
val sheareYZ : distance : float -> Transformation
val sheareZX : distance : float -> Transformation
val sheareZY : distance : float -> Transformation
val scale : x : float -> y : float -> z : float -> Transformation
val mirrorX : Transformation
val mirrorY : Transformation
val mirrorZ : Transformation

val mergeTransformations : Transformation list -> Transformation