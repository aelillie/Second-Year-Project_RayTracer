
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


//Save file with path and name of file
let saveFile path name (bmap:Bitmap) : unit = 
    let fullpath = path + name
    bmap.Save(fullpath)

//Handler when clicking save
let saveFileHandler (bmap:Bitmap) = 
    let sf = new SaveFileDialog()   
    sf.FileName <- "image.jpg"      //initial save name
    sf.AddExtension <- true         //Set up extensions      
    sf.Filter <- "Bitmap Image (.bmp)|*.bmp|Gif Image (.gif)|*.gif|JPEG Image (.jpeg)|*.jpeg|Png Image (.png)|*.png|Tiff Image (.tiff)|*.tiff|Wmf Image (.wmf)|*.wmf"
    //Show dialog and set what happens when ok is clicked.
    if sf.ShowDialog(new Form(Text="Save", TopMost=true, Width=360, Height=390)) = System.Windows.Forms.DialogResult.OK 
                                            then saveFile (sf.InitialDirectory) (sf.FileName) (bmap)
                                            else printfn "Error couldn't save"

//Creates the window to show 
let mkWindow (bmap:Bitmap) = 
    //Create form holding image.
    let form = 
        //Main form
        let temp = new Form(Width = bmap.Width, Height = bmap.Height)
        //PictureBox for holding bitmap
        let pb = new PictureBox()
        pb.Image <- bmap
        pb.SizeMode <- PictureBoxSizeMode.AutoSize
        //Set up menu and save button
        let menu = new MainMenu()
        let save = new MenuItem("Save")
        save.Visible <- true
        
        //Set up handler for when save is clicked
        save.Click.Add(fun evArgs -> saveFileHandler bmap )
        menu.MenuItems.Add(save) |> ignore
        
        //Add all to main form
        temp.Controls.Add pb
        temp.Menu <- menu
        temp.Visible <- true
        temp
    form   

             
    


 