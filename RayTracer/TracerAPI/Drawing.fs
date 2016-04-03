module Drawing
open TracerAPI
open System
open System.Drawing


let width = 480
let height = 320


type Result = {x:int; y:int; color:Color}

let resultList = [{x = 3;y = 3;color = Color.Red},{x = 4;y = 4;color = Color.Black},{x = 5;y = 5;color = Color.Blue}]

//Makes a picture by using list provided by the Camera
let mkPicture list (bmp:Bitmap)=
    let rec mkPictureRec list =
        match list with 
        | []    -> bmp
        | x::xs -> bmp.SetPixel(x) 
                   mkPictureRec xs 

    let bmp = mkPictureRec list
    bmp.Save("output.jpg")
