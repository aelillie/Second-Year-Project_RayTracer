module Tests
open System
open Shape
open BoundingBox
open Point 
open System.Drawing


let s = mkSphere (mkPoint 2.0 2.0 -1.2) 1.0 (Material.mkMaterial (Colour.fromColor Color.Green) 0.5)
let bs = calc s




