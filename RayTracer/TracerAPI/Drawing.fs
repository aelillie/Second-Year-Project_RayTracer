module Drawing
open Tracer
open System
open System.Drawing
open Colour
open System.Windows.Forms

///Draws a picture from a list of coordinates and a list of
///information about how each pixel should be rendered
let mkPicture viewPlane resX resY =

    let bmp = new Bitmap(resX+10,resY+10)
    let rec mkPictureRec vP =
        match vP with 
        | []    -> bmp
        | x::xs -> bmp.SetPixel(x) 
                   mkPictureRec xs 
    let bmp = mkPictureRec viewPlane
    bmp


let mkWindow (bmap:Bitmap) = 
    
    let form = 
        let temp = new Form(Width = bmap.Width, Height = bmap.Height)
        temp.BackgroundImage <- bmap
        let menu = new MenuStrip()
        let save = new ToolStripMenuItem()
        menu.Items.Add(save) |> ignore
        
        temp.Controls.Add menu
        temp.Visible <- true
        System.Windows.Forms.Application.Run temp
    form


 