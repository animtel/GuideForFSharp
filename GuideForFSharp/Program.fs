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

let fetchUrl callback url =        
    let req = WebRequest.Create(Uri(url)) 
    use resp = req.GetResponse() 
    use stream = resp.GetResponseStream() 
    use reader = new IO.StreamReader(stream) 
    callback reader url

let myCallback (reader:IO.StreamReader) url = 
        let html = reader.ReadToEnd()
        //printfn "Downloaded %s. First 1000 is %s" url html
        html      // return all the html

let testGetData = fetchUrl myCallback "http://google.com"

let getFileInfo filePath =
        let fi = new System.IO.FileInfo(filePath)
        if fi.Exists then Some(fi) else None

let goodFileName = @"C:\Users\danil.kovalenko\Desktop\New Text Document.txt"

let goodFileInfo = getFileInfo goodFileName // Some(fileinfo)

let CheckFileExisting path = 
    let fi = new System.IO.FileInfo(path)
    match fi.Exists with
    | true -> 
        true
    | false -> 
        false

let rec movingAverages list = 
    match list with
    | [] -> []
    | x::y::rest -> 
        let avg = (x+y)/2.0 
        avg :: movingAverages (y::rest)
    | [_] -> []

// define a "union" of two different alternatives
type Result<'a, 'b> = 
    | Success of 'a  // 'a means generic type. The actual type
                     // will be determined when it is used.
    | Failure of 'b  // generic failure type as well

// define all possible errors
type FileErrorReason = 
    | FileNotFound of string
    | UnauthorizedAccess of string * System.Exception

// define a low level function in the bottom layer
let performActionOnFile action filePath =
   try
      //open file, do the action and return the result
      use sr = new System.IO.StreamReader(filePath:string)
      let result = action sr  //do the action to the reader
      sr.Close()
      Success (result)        // return a Success
   with      // catch some exceptions and convert them to errors
      | :? System.IO.FileNotFoundException as ex 
          -> Failure (FileNotFound filePath)      
      | :? System.Security.SecurityException as ex 
          -> Failure (UnauthorizedAccess (filePath,ex))  
      // other exceptions are unhandled

// a function in the middle layer
let middleLayerDo action filePath = 
    let fileResult = performActionOnFile action filePath
    // do some stuff
    fileResult //return

// a function in the top layer
let topLayerDo action filePath = 
    let fileResult = middleLayerDo action filePath
    // do some stuff
    fileResult //return

/// get the first line of the file
let printFirstLineOfFile filePath = 
    let fileResult = topLayerDo (fun fs->fs.ReadLine()) filePath

    match fileResult with
    | Success result -> 
        // note type-safe string printing with %s
        printfn "first line is: '%s'" result   
    | Failure reason -> 
       match reason with  // must match EVERY reason
       | FileNotFound file -> 
           printfn "File not found: %s" file
       | UnauthorizedAccess (file,_) -> 
           printfn "You do not have access to the file: %s" file

let printLengthOfFile filePath = 
   let fileResult = 
     topLayerDo (fun fs->fs.ReadToEnd().Length) filePath

   match fileResult with
   | Success result -> 
      // note type-safe int printing with %i
      printfn "length is: %i" result       
   | Failure _ -> 
      printfn "An error happened but I don't want to be specific"



[<EntryPoint>]
let main argv =
    printfn "Hello World"
    printfn ""
    //Test functions REGION
    //-------------------------------------------------------------------------------------------------------------------------------------------------------

    let listNumbers = [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;10.0]
    printfn "%A" (movingAverages listNumbers)

    /// write some text to a file
    let writeSomeText filePath someText = 
        use writer = new System.IO.StreamWriter(filePath:string)
        writer.WriteLine(someText:string)
        writer.Close()
    
    let goodFileName = "C:\Users\danil.kovalenko\Desktop\New Text Document.txt"

    writeSomeText goodFileName testGetData


    //-------------------------------------------------------------------------------------------------------------------------------------------------------
    //Test functions REGION
    printfn ""
    0 // return an integer exit code
