open TracerAPI
open System
open System.Drawing


let width = 480
let height = 320

let bmp = new Bitmap(width,height)

type Result = {x:int; y:int; color:Color}

let resultList = [{x = 3;y = 3;color = Color.Red},{x = 4;y = 4;color = Color.Black},{x = 5;y = 5;color = Color.Blue}]

//Makes a picture by using list provided by the Camera
let rec mkPicture list =
<<<<<<< HEAD
    match list with
    | []   -> []
    | x::xs -> bmp.SetPixel(x)
               mkPicture xs
               
=======
    match list with 
    | []    -> bmp
    | x::xs -> bmp.SetPixel(x) 
               mkPicture xs 
>>>>>>> 1c89b721af60cc34238c378701445a69f369daed
bmp.Save("output.jpg")
