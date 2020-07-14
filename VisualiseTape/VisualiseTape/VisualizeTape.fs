#light

module Sim900.VisualizeTape

open System.IO

let Trim (buffer: byte[]) =
    let rec Helper left right =
        if   left >= buffer.Length
        then [||]
        elif buffer.[left] = 0uy
        then Helper (left+1) right
        elif buffer.[right] = 0uy
        then Helper left (right-1)
        else buffer.[left..right]
    Helper 0 (buffer.Length-1)

let ArgList () = 
    let commandLine = System.Environment.GetCommandLineArgs();
    if   commandLine.Length < 2
    then failwith "Usage: VisualizeTape file [inches]"
    if   not (commandLine.[1].ToUpper().EndsWith("RAW"))
    then failwith "Input is not a RAW file"
    let input = 
        try File.ReadAllBytes commandLine.[1]
        with ex -> failwith (sprintf "Cannot open input file: %s" ex.Message)
    let inches = 
        if commandLine.Length = 2
        then 10.0
        else try (float commandLine.[2])
                with ex -> failwith (sprintf "Incorrect inches parameter: %s" ex.Message)
    (input, inches)

let Legible (buffer: byte[]) inches =
    for i = 0 to (min (int inches*10) (buffer.Length-1)) do
        let code = (int buffer.[i])
        for row in seq[128; 64; 32; 16; 8; 0; 4; 2; 1] do 
            // print bit patterns
            printf (if   row = 0 then "o" elif (code &&& row) = 0 then " " else "O")
        printf " (%3d)" code
        if i % 10 = 0 && i <> 0 then printf " %4d\"" (i/10)
        printfn ""

try
    let input, inches = ArgList ()
    Legible (Trim input) inches
with ex -> stdout.WriteLine ex.Message
                  

