module PlyParse

open FParsec
open System.IO


type UserState = unit
type Ply = 
         | Vertex of float * float * float
         | Face of int list
         | Comment of string
         | DummyData
         | Element of string * int
         | Endheader 



type Parser<'t> = Parser<'t, UserState>    

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}





let pComment: Parser<_> =  pstring "comment" >>. restOfLine true |>> (fun x -> Comment x)

let pEndHeader: Parser<_> = pstring "end_header" .>> skipRestOfLine true |>> (fun _ -> Endheader)

let pVertex: Parser<_> = pfloat .>>. (spaces >>. pfloat) .>>. (spaces >>. pfloat) |>> (fun ((x,y),z) -> Vertex (x,y,z))

let pIntC: Parser<_> = pint32

let pFace: Parser<_> = pint32 >>? spaces1 >>. many1 (pint32 .>> spaces1) |>> (fun x -> Face (x))

let pDummyData: Parser<_> = skipRestOfLine true |>> (fun _ -> DummyData)

let pElement: Parser<_> = pstring "element" >>. spaces >>. (many1Chars anyChar) .>> spaces .>>.  pint32 |>> (fun (x,f) -> Element (x,f))


let pPly: Parser<_> = (pComment <|> pEndHeader) <|> (attempt pFace <|> pVertex) <|> (pElement <|> pDummyData)


let pPlys: Parser<_> = many (pPly .>> spaces)

let k p = run pPlys p



let parse p str = 
    match run p str with
     |Success(result,_,_) -> result
     |Failure(er,_,_) -> DummyData
          


let parsePly filePath =

    let lines = readLines filePath
    
    Seq.map (fun x -> parse pPly x) lines |> Seq.toList
    
    



