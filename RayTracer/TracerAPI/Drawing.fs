module Drawing
open TracerAPI
open System
open System.Drawing
open Colour

///Draws a picture from a list of coordinates and a list of
///information about how each pixel should be rendered
let mkPicture coords viewPlane resX resY=

    let render ((x:int),(y:int)) = function
     | None -> (x,y, System.Drawing.Color.White)
     | Some (_,_,c) -> (x,y,Colour.toColor c)

    let vP = List.map2 render coords viewPlane

    let bmp = new Bitmap(resX+10,resY+10)
    let rec mkPictureRec vP =
        match vP with 
        | []    -> bmp
        | x::xs -> bmp.SetPixel(x) 
                   mkPictureRec xs 
    let bmp = mkPictureRec vP
    bmp.Save("RayTracer.jpg")
