///Transformation matrices for object manipulation
module Transformation
open Point
open Vector

type Transformation

///Get the normal transformation matrix
val getT : Transformation -> float[,]
///Get the inversion transformation matrix
val getInv : Transformation -> float[,]

///Transpose a transformation matrix
///by swapping all rows with columns
val transpose : float[,] -> float[,]
///Multiply a transformation matrix with some point.
///Returns a new point, with same dimensions, but
///with updated values
val transPoint : float[,] -> Point -> Point
///Does the same as transPoint, but runs in parallel
val transPointParallel : float[,] -> Point -> Point
///Multiply a transformation matrix with some vector.
///Returns a new vector, with same dimensions, but
///with updated values
val transVector : float[,] -> Vector -> Vector
///Does the same as transVector, but runs in parallel
val transVectorParallel : float[,] -> Vector -> Vector

///Move a shape in some direction/dimension
val translate : x : float -> y : float -> z : float -> Transformation
///Scale a shape with a factor n in some direction/dimension
val scale : x : float -> y : float -> z : float -> Transformation
///Mirror shape around the x-axis
val mirrorX : Transformation
///Mirror shape around the y-axis
val mirrorY : Transformation
///Mirror shape around the z-axis
val mirrorZ : Transformation
///Rotate shape around the x-axis by the given angle
///measured in radians
val rotateX : angle : float -> Transformation
///Rotate shape around the x-axis by the given angle
///measured in degrees
val rotateX1 : angle : float -> Transformation
///Rotate shape around the y-axis by the given angle
///measured in radians
val rotateY : angle : float -> Transformation
///Rotate shape around the x-axis by the given angle
///measured in degrees
val rotateY1 : angle : float -> Transformation
///Rotate shape around the z-axis by the given angle
///measured in radians
val rotateZ : angle : float -> Transformation
///Rotate shape around the x-axis by the given angle
///measured in degrees
val rotateZ1 : angle : float -> Transformation
///Shear shape some distance with respect
///to the x- and y-axis
val sheareXY : distance : float -> Transformation
///Shear shape some distance with respect
///to the x- and z-axis
val sheareXZ : distance : float -> Transformation
///Shear shape some distance with respect
///to the y- and x-axis
val sheareYX : distance : float -> Transformation
///Shear shape some distance with respect
///to the y- and z-axis
val sheareYZ : distance : float -> Transformation
///Shear shape some distance with respect
///to the z- and x-axis
val sheareZX : distance : float -> Transformation
///Shear shape some distance with respect
///to the z- and y-axis
val sheareZY : distance : float -> Transformation

///Merge the given list of transformations into one, such that the resulting
///transformation is equivalent to applying the individual transformations
///from left to right (i.e. starting with the first element in the list).
val mergeTransformations : Transformation list -> Transformation