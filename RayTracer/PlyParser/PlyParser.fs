﻿module PlyParse

open FParsec
open System.IO


type UserState = unit
type Ply = 
         | Vertex of float list
         | Property of string
         | Face of int list
         | Comment of string
         | DummyData of string
         | Element of string * int * Ply list
         | Endheader 
         override p.ToString() =
          match p with
           Property x -> x
          |_ -> "Override rest of elements if needed"


let print p writer = 
      match p with
         | Vertex x -> fprintf writer "Vertex %O \n" x
         | Face (x) -> fprintf writer "Face %O \n" x
         | Comment x -> fprintf writer "Comment %s \n" x
         | DummyData x -> fprintf writer "DummyData %s \n" x
         | Element (e, n, pr) -> fprintf writer "Element %s %d %O \n" e n pr
         | Endheader -> fprintf writer "end_header \n"
         | Property s -> fprintf writer "Property %s \n" s


type Parser<'t> = Parser<'t, UserState>    

//Reads all lines of a file and rturns them as a sequence.
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


//Helper parsers
let pNameString: Parser<_> = satisfy (fun c ->  System.Char.IsLetterOrDigit c)
let pList n : Parser<_> = if n > 0 then (parray n (pint32 .>> spaces)) else failFatally "lol"


// Set up parsers for each type  
let pComment: Parser<_> =  pstring "comment" >>. restOfLine true |>> (fun x -> Comment x)
let pEndHeader: Parser<_> = pstring "end_header" .>> skipRestOfLine true |>> (fun _ -> Endheader)
let pVertex: Parser<_> = spaces >>. many1 (pfloat .>> spaces) .>> spaces |>> (fun x -> Vertex x)
let pFace: Parser<_> = pint32 .>>? spaces1 >>=? pList |>> (fun x -> Face (Array.toList x))
let pDummyData: Parser<_> = restOfLine true |>> (fun x -> DummyData x)
let pElement: Parser<_> = pstring "element" >>. spaces >>. (many1Chars pNameString) .>> spaces .>>.  pint32 |>> (fun (x,f) -> Element (x,f,List.empty))
let pProperty: Parser<_> = pstring "property" >>. spaces >>. (many1Chars pNameString) .>> spaces >>.  restOfLine false |>> (fun x -> Property x)

// Create a parser which can choose from each of them
let pPly2: Parser<_> = choice [pComment 
                               pElement 
                               pEndHeader 
                               pProperty
                               attempt pFace 
                               pVertex 
                               pDummyData]

// Helper method returning the result (ply type) or DummyData if failure
let parse p str = 
    match run p str with
     |Success(result,_,_) -> result
     |Failure(er,_,_) -> DummyData er

//Parse header of PlyFile
let rec parseHeader xs = 
    let rec parseProperties xs = 
     match xs with
     | x::xs' -> match parse pPly2 x with
                 |Property x as p -> p::parseProperties xs'
                 |_ -> []
     |[] -> []

    let top = List.head xs
    match parse pPly2 top with
         | Comment x as c-> c::parseHeader (List.tail xs)
         | DummyData x as d-> d::parseHeader (List.tail xs)
         | Element (el, n, pr) -> Element(el, n, parseProperties (List.tail xs))::parseHeader (List.tail xs) //Create property list.
         | Property s as p-> parseHeader (List.tail xs)
         | Endheader as eh -> [eh]          //Return header
         | _ -> failwith "Shouldn't reach this"
         
//To filter out header elements.
let isHeader ply = 
    match ply with
    | Comment x as c-> true
    | DummyData x as d-> true
    | Element (el, n, pr) as e -> true
    | Property s as p-> true
    | Endheader as eh -> true
    | _ -> false
              

let parsePly filePath =

    let lines = Seq.toList (readLines filePath) // read lines
    let header = parseHeader lines
    let body = List.filter (fun x -> not (isHeader x)) (List.map (fun x -> parse pPly2 x) lines)
    List.append header body  //Return complete list.

    
    
    


