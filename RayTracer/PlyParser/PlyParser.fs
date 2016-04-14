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

//Reads all lines of a file and rturns them as a sequence.
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


// Set up parsers for each type  
let pComment: Parser<_> =  pstring "comment" >>. restOfLine true |>> (fun x -> Comment x)
let pEndHeader: Parser<_> = pstring "end_header" .>> skipRestOfLine true |>> (fun _ -> Endheader)
let pVertex: Parser<_> = pfloat .>>. (spaces >>. pfloat) .>>. (spaces >>. pfloat) |>> (fun ((x,y),z) -> Vertex (x,y,z))
let pFace: Parser<_> = pint32 >>? spaces1 >>. many1 (pint32 .>> spaces1) |>> (fun x -> Face (x))
let pDummyData: Parser<_> = skipRestOfLine true |>> (fun _ -> DummyData)
let pElement: Parser<_> = pstring "element" >>. spaces >>. (many1Chars anyChar) .>> spaces .>>.  pint32 |>> (fun (x,f) -> Element (x,f))

// Create a parser which can choose from each of them
let pPly: Parser<_> = (pComment <|> pEndHeader) <|> (attempt pFace <|> pVertex) <|> (pElement <|> pDummyData)

// Helper method returning the result (ply type) or DummyData if failure
let parse p str = 
    match run p str with
     |Success(result,_,_) -> result
     |Failure(er,_,_) -> DummyData
          

let parsePly filePath =

    let lines = readLines filePath // read lines
    
    Seq.map (fun x -> parse pPly x) lines |> Seq.toList // parse all lines and map them
    
    



