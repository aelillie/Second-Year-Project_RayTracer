module Kdtree
open BoundingBox
open Point

type Kdtree
type KdNode
type hyperRect
type part   
type nearestNeighbor 

val mkKdtree : Point list -> hyperRect -> Kdtree
val findNearestNeighbor : KdNode -> Point -> hyperRect
val insert : Point list -> KdNode -> int -> Kdtree 
   

