module BoundingBoxTest

open System
open Shape
open BoundingBox
open Point 
open System.Drawing


let doTest() =
    let s = mkSphereCenter 1.0 (Material.mkMaterial (Colour.fromColor Color.Green) 0.5)
    let bs = calc s

    Console.WriteLine(bs.ToString())

