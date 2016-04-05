module Material
open System.Drawing
open Colour
  
type Material =
  | M of Colour*float
  override m.ToString() =
    match m with
      M (c,r) -> "["+c.ToString()+","+r.ToString()+"]"

let mkMaterial (c : Colour) (r : float) : Material = M(c,r)


let getColour (M(c, f)) = c

