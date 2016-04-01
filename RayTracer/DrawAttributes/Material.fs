module Material
open System.Drawing

type Colour =
  | C of Color
  
type Material =
  | M of Colour*float
  override m.ToString() =
    match m with
      M (c,r) -> "["+c.ToString()+","+r.ToString()+"]"

let mkColour (r : float) (g : float) (b : float) : Colour = 
    let checkCeiling c = 
        match c with
        | _ when c>1.0 -> 1.0
        | _ when c<0.0 -> 0.0
        | _ -> c
    let convert c = System.Convert.ToInt32((checkCeiling c) *255.0)    
    C (Color.FromArgb(255, convert r, convert g, convert b))

let fromColor (c : Color) : Colour = C (c) 
let getColour (M(C(c), f)) = c
let mkMaterial (c : Colour) (r : float) : Material = M(c,r)
