namespace TestSuite

open System.IO
open Scene
open System

module Util =
  let degrees_to_radians (d : float) = d * Math.PI / 180.0


  let private source_path = "../../.."
  let private ply_path = source_path + "/ply"
  let private result_path = source_path + "/result"

  let render s (toFile : (string*string) option) =
    match toFile with
    | Some (f, fn) -> 
      let path = if f = "" then result_path else result_path + "/" + f
      Directory.CreateDirectory path |> ignore
      renderToFile s (path + "/" + fn)
    | None -> renderToScreen s

  let render' (s : Scene) (toFile : (string*string)) (toScreen : bool) = 
    if toScreen
    then render s None
    else render s (Some toFile)
  
  