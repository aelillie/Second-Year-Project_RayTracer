module colour
open System.Drawing

type colour =
  | C of Color
  



let mkColour (r : float) (g : float) (b : float) : colour = 

    let checkCeiling c = match c with
        | _ when c>1.0 -> 1.0
        | _ when c<0.0 -> 0.0
        | _ -> c
    
    let rColor = System.Convert.ToInt32((checkCeiling r) *255.0)
    let gColor = System.Convert.ToInt32((checkCeiling g) *255.0)
    let bColor = System.Convert.ToInt32((checkCeiling b) *255.0)
    
    C (Color.FromArgb(255, rColor, gColor, bColor))



let fromColor (c : Color) : colour = C (c)