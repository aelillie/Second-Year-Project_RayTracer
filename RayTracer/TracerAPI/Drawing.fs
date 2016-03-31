open TracerAPI
open System
open System.Drawing


let width = 480
let height = 320

let bmp = new Bitmap(width,height)

//Makes a picture by using list provided by the Camera
let rec mkPicture list =
    match list with 
    | []   -> []
    | x::xs -> bmp.SetPixel(x) 
               mkPicture xs 
bmp.Save("output.jpg")
