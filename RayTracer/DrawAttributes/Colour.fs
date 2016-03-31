module colour


type colour =
  | C of float * float * float
  override c.ToString() =
    match v with
      C(x,y,z) -> "["+x.ToString()+","+y.ToString()+","+z.ToString()+"]"


let mkColour (r : float) (g : float) (b : float) : colour = C (r, g, b)
let fromColor (c : System.Drawing.Color) : colour = 



