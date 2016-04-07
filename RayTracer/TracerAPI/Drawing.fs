module Drawing
open Tracer
open System
open System.Drawing
open Colour

///Draws a picture from a list of coordinates and a list of
///information about how each pixel should be rendered
let mkPicture viewPlane resX resY=

    let bmp = new Bitmap(resX+10,resY+10)
    let rec mkPictureRec vP =
        match vP with 
        | []    -> bmp
        | x::xs -> bmp.SetPixel(x) 
                   mkPictureRec xs 
    let bmp = mkPictureRec viewPlane
    bmp.Save("RayTqeracer.jpg")
