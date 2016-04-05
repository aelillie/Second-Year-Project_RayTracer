module Drawing
open TracerAPI
open System
open System.Drawing
open Colour


let width = 480
let height = 320


type Result = {x:int; y:int; color:Color}

let resultList = [{x = 3;y = 3;color = Color.Red},{x = 4;y = 4;color = Color.Black},{x = 5;y = 5;color = Color.Blue}]

//Makes a picture by using list provided by the Camera
let mkPicture xy l (px:int) (py:int)=
    
    let setColor ((x:int),(y:int)) = function
     | None -> (x,y, System.Drawing.Color.White)
     | Some (_,_,c) -> (x,y,Colour.toColor c)

    let p = List.map2 setColor xy l
    
    let bmp = new Bitmap(px+10,py+10)
    let rec mkPictureRec list =
        match list with 
        | []    -> bmp
        | x::xs -> bmp.SetPixel(x) 
                   mkPictureRec xs 
    let bmp = mkPictureRec p
    bmp.Save("output.jpg")
