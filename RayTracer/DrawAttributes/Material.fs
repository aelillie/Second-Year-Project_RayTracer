module Material
open colour

type material =
  | M of colour*float
  override m.ToString() =
    match m with
      M (c,r) -> "["+c.ToString()+","+r.ToString()+"]"

 

let mkMaterial (c : colour) (r : float) : material = M(c,r)
