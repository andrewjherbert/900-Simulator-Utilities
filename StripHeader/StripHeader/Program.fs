#light

module StripHeader

    open System.IO


// StripHeader inputFile inches
// Input must be be RAW, output is BIN

    // Trim a buffer of leading and trailing blanks
    let Trim (buffer: byte[]) =
        let rec Helper left right =
            if   left = buffer.Length
            then [||]
            elif buffer.[left] = 0uy
            then Helper (left+1) right
            elif buffer.[right] = 0uy
            then Helper left (right-1)
            else buffer.[left..right]
        Helper 0 (buffer.Length-1)

    // Copy a buffer to output in BIN format
    let Copy (buffer: byte[]) =
        let Print count code =
            if   count % 20 = 0
            then printfn "%4d" code
            else printf  "%4d" code
        let rec Helper count index =
            if   index < buffer.Length
            then Print count buffer.[index]
                 Helper (count+1) (index+1)
            else count
        let rec Runout count blanks =
            if   blanks > 0
            then Print count 0uy
                 Runout (count+1) (blanks-1)
            else count
        let count1 = Runout 1 80
        let count2 = Helper count1 0
        Runout count2 80 |> ignore

    // Output legible header from buffer as a comment
    let Legible (buffer: byte[]) =
        for i in seq[ 1uy; 2uy; 4uy; 8uy; 16uy; 32uy; 64uy; 128uy ] do
            stdout.Write "( Legible Header "
            for row in 0..buffer.Length-1 do
                stdout.Write (if (buffer.[row] &&& i) <> 0uy then " 0" else  "  ")
            stdout.WriteLine " )"
        stdout.WriteLine ()

    // Main Program

    // Decode command line
    let commandLine = System.Environment.GetCommandLineArgs();
    if   commandLine.Length < 3
    then printfn "Command Line error: StripHeader input inches"
    else // parameter validation
         let inputFileName  = commandLine.[1]
         if   not (inputFileName.ToUpper().EndsWith(".RAW"))
         then failwith (sprintf "File \"%s\" is not a RAW file" inputFileName)
         let inches = try (float commandLine.[2]) with ex -> 
                        failwith (sprintf "Problem with inches parameter: %s" ex.Message)
         let rows   = (int inches*10)
           
         // read input file
         let buffer = 
            Trim (try File.ReadAllBytes(inputFileName) 
                  with ex -> failwith (sprintf "Cannot read file \"%s\": %s" inputFileName ex.Message))
         // deal with header
         Legible (Trim (buffer.[..rows])) // trim off any blanks at end
         // deal with rest of file
         Copy (Trim buffer.[rows+1..])
            

