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

let file = @"C:\Users\i5-4670K\Documents\airplane.ply.txt"

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

let pElement: Parser<_> = pstring "element" >>. spaces >>. manyChars anyChar .>> spaces .>>.  pint32 |>> (fun (x,f) -> Element (x,f))


let pPly: Parser<_> = (pComment <|> pEndHeader) <|> (attempt pFace <|> pVertex) <|> pElement <|> pDummyData


let pPlys: Parser<_> = many (pPly .>> spaces)

let k p = run pPlys p



let parse p str = 
    match run p str with
     |Success(result,_,_) -> result
     |Failure(er,_,_) -> DummyData
          


let parsePly filePath =

    let lines = readLines filePath
    
    let s = Seq.map (fun x -> parse pPly x) lines |> Seq.toList


    let writer = new StreamWriter (@"C:\Users\i5-4670K\Documents\test.txt")

    let print p = 
        match p with
         | Vertex (x,y,z) -> fprintf writer "Vertex %f %f %f \n" x y z
         | Face (x) ->       fprintf writer "Face %O" x
         | Comment x -> fprintf writer "Comment %s \n" x
         | DummyData -> fprintf writer "DummyData \n"
         | Element (e, n) -> fprintf writer "Element %s, %d \n" e n
         | Endheader -> fprintf writer "end_header \n"


    Seq.iter(fun x -> print x) s


