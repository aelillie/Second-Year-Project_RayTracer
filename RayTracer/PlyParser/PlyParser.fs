module PlyParse

open FParsec
open System.IO


type UserState = unit
type Ply = 
         | Vertex of float * float * float
         | Vertex2 of float * float * float * float * float
         | Vertex3 of float * float * float * float * float * float * float * float
         | Face of int list
         | Comment of string
         | DummyData of string
         | Element of string * int
         | Endheader 


let print p writer = 
      match p with
         | Vertex (x,y,z) -> fprintf writer "Vertex %f %f %f \n" x y z
         | Face (x) ->       fprintf writer "Face %O \n" x
         | Comment x -> fprintf writer "Comment %s \n" x
         | DummyData x -> fprintf writer "DummyData %s \n" x
         | Element (e, n) -> fprintf writer "Element %s %d \n" e n
         | Endheader -> fprintf writer "end_header \n"
         | Vertex2 (x,y,z,u,v) -> fprintf writer "Vertex %f %f %f %f %f \n" x y z u v
         | Vertex3 (x,y,z,nx,ny,nz,u,v) -> fprintf writer "Vertex %f %f %f %f %f %f %f %f \n" x y z nx ny nz u v

type Parser<'t> = Parser<'t, UserState>    

//Reads all lines of a file and rturns them as a sequence.
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let mkVertex n (xs:float list) =
    match n with
    |3 -> Vertex((xs.Item 0),xs.Item 1,xs.Item 2)  
    |5 -> Vertex2((xs.Item 0),xs.Item 1,xs.Item 2,(xs.Item 3),xs.Item 4)  
    |8 -> Vertex3((xs.Item 0),xs.Item 1,xs.Item 2,(xs.Item 3),xs.Item 4,(xs.Item 5),xs.Item 6,xs.Item 7)  
    |_ -> failwith "Encounted an errornous number of values when creating a vertex of %d, %O" n xs
    
//Helper parsers
let pElementName: Parser<_> = satisfy (fun c ->  System.Char.IsLetterOrDigit c)
let pList n : Parser<_> = if n > 0 then (parray n (pint32 .>> spaces)) else failFatally "lol"


// Set up parsers for each type  
let pComment: Parser<_> =  pstring "comment" >>. restOfLine true |>> (fun x -> Comment x)
let pEndHeader: Parser<_> = pstring "end_header" .>> skipRestOfLine true |>> (fun _ -> Endheader)
let pVertex: Parser<_> = spaces >>. many1 (pfloat .>> spaces) .>> spaces |>> (fun x -> mkVertex x.Length x)
//let pVertex: Parser<_> = pfloat .>>. (spaces >>. pfloat) .>>. (spaces >>. pfloat) |>> (fun ((x,y),z) -> Vertex (x,y,z))
//let pFace: Parser<_> = pint32 .>>? spaces1 >>= pList .>> spaces |>> (fun x -> mkFace x)
let pFace: Parser<_> = pint32 .>>? spaces1 >>=? pList |>> (fun x -> Face (Array.toList x))
//let pFace: Parser<_> = pint32 .>>? spaces1 .>>. many1 (pint32 .>> spaces1) |>> (fun (y,x) -> Face (x))
let pDummyData: Parser<_> = restOfLine true |>> (fun x -> DummyData x)
let pElement: Parser<_> = pstring "element" >>. spaces >>. (many1Chars pElementName) .>> spaces .>>.  pint32 |>> (fun (x,f) -> Element (x,f))


// Create a parser which can choose from each of them
let pPly2: Parser<_> = choice [pComment 
                               pElement 
                               pEndHeader 
                               attempt pFace 
                               pVertex 
                               pDummyData]
// Helper method returning the result (ply type) or DummyData if failure
let parse p str = 
    match run p str with
     |Success(result,_,_) -> result
     |Failure(er,_,_) -> DummyData er
          

let parsePly filePath =

    let lines = readLines filePath // read lines
    
    Seq.map (fun x -> parse pPly2 x) lines |> Seq.toList // parse all lines and map them
    
    



