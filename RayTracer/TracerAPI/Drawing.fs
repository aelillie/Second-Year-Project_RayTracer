
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

let saveFile path name (bmap:Bitmap) : unit = 
    let fullpath = path + name
    bmap.Save(fullpath)


let saveFileHandler (bmap:Bitmap) = 
    let sf = new SaveFileDialog()
    sf.FileName <- "image.jpg"
    sf.AddExtension <- true
    sf.Filter <- "Bitmap Image (.bmp)|*.bmp|Gif Image (.gif)|*.gif|JPEG Image (.jpeg)|*.jpeg|Png Image (.png)|*.png|Tiff Image (.tiff)|*.tiff|Wmf Image (.wmf)|*.wmf"
    sf.FileOk.Add(fun evArgs -> bmap.Save(sf.FileName))
    if sf.ShowDialog(new Form(Text="Save", TopMost=true, Width=360, Height=390)) = System.Windows.Forms.DialogResult.OK 
                                            then saveFile (sf.InitialDirectory) (sf.FileName) (bmap)
                                            else printfn "Error couldn't save"

//Shows the window 
let mkWindow (bmap:Bitmap) = 
    let form = 
        let temp = new Form(Width = bmap.Width, Height = bmap.Height)
        let pb = new PictureBox()
        pb.Image <- bmap
        pb.SizeMode <- PictureBoxSizeMode.AutoSize
        let menu = new MenuStrip()
        let save = new ToolStripMenuItem()
        save.Width <- 20
        save.Click.Add(fun evArgs -> saveFileHandler bmap )
        menu.Items.Add(save) |> ignore
        menu.AutoSize <- true
        temp.Controls.Add menu

        temp.Controls.Add pb
        temp.Visible <- true
        temp
        
        //System.Windows.Forms.Application.Run temp
    form
    


 