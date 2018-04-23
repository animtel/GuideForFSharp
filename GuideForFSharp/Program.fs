// Learn more about F# at http://fsharp.org

open System.Net
open System
open System.IO

let ConcatTwoStrings x y = 
    x + y

let rec fib n =
    match n with
    | 1 | 2 -> 1
    | n -> fib(n-1) + fib(n-2)

let evens list = // it's function for get filter for list; if lsit[i]%2 == 0 we are get it; else - delete
   let isEven x = x%2 = 0     
   List.filter isEven list    

let ConvertToString list = 
    list 
    |> List.map (fun i -> i.ToString()) 
    |> String.concat ", "

let Square x =
    x * x

let SumSquaresToX x =
    List.sum ( List.map Square [1..x] )

let SumSquaresToXTwo x = 
    [1..x]
    |> List.map Square
    |> List.sum

let SumSquaresToXThree x =
    [1..x]
    |> List.map (fun y -> y*y)
    |> List.sum

let SimplePatternMatch x =
   match x with
    | "a" -> "x is a"
    | "b" -> "x is b"
    | _ -> "x is something else" // underscore matches anything

let OptionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

type Person = {First:string; Last:string}

let PersonContr firstName lastName =
    {First=firstName; Last=lastName}

let product n = 
    let initialValue = 1
    let action productSoFar x = productSoFar * x
    [1..n] |> List.fold action initialValue

let sumOfOdds n = 
    let initialValue = 0
    let action sumSoFar x = if x%2=0 then sumSoFar else sumSoFar+x 
    [1..n] |> List.fold action initialValue

let alternatingSum n = 
    let initialValue = (true,0)
    let action (isNeg,sumSoFar) x = if isNeg then (false,sumSoFar-x)
                                             else (true ,sumSoFar+x)
    [1..n] |> List.fold action initialValue |> snd

type Temp = 
    | DegreesC of int
    | Name of string

let rec quicksort list =
   match list with
   | [] ->                            
        []                            
   | firstElem::otherElements ->      
        let smallerElements =         
            otherElements             
            |> List.filter (fun e -> e < firstElem) 
            |> quicksort              
        let largerElements =           
            otherElements 
            |> List.filter (fun e -> e >= firstElem)
            |> quicksort              
        List.concat [smallerElements; [firstElem]; largerElements]

let rec quicksort2 = function
   | [] -> []                         
   | first::rest -> 
        let smaller,larger = List.partition ((>=) first) rest 
        List.concat [quicksort2 smaller; [first]; quicksort2 larger]

[<EntryPoint>]
let main argv =
    printfn "Hello World"
    printfn ""
    //Test functions REGION
    //-------------------------------------------------------------------------------------------------------------------------------------------------------




    //-------------------------------------------------------------------------------------------------------------------------------------------------------
    //Test functions REGION
    printfn ""
    0 // return an integer exit code
