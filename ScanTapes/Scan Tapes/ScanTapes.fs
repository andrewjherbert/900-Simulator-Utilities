#light

module Sim900.ScanTapes

    open System.Collections

// SCANTAPES fromDir toDir
//
// Read a folder of Elliott eight track paper tape images and convert to UTF encoded files.
// Assumes each file in the hierarchy is in form <name>.1[, <name>.2[, <name>.3]] representing multiple
// reads of same tape, or reads of copies of tape.  A simple sumcheck test is done to ensure
// these are all the same.
// 
// The source folder can contain sub folders and this structure is replicated in the target folder.
// 
// All files are output as <name>.RAW in the destination folder exactly as in the source file.  Files are
// then scanned to step over any legible header, then an atempt is made to recognize text files in either 920 
// or 900/903 telecodes by looking for an initial newline or carriage return (900 telecode only).  If a file 
// appears to be text then a .920 or .900 file is created as appropriate.  Otehrwise the file is assumed to
// be binary and a .BIN file is created.
// 

    open System.IO

    let DirsIn name = // list all directories in a directory
        Seq.map (fun (d: DirectoryInfo) -> d.FullName) ((System.IO.DirectoryInfo name).EnumerateDirectories ())

    let FilesIn name = // list all files in a directory
        Seq.map (fun (f: FileInfo) -> f.FullName) ((System.IO.DirectoryInfo name).EnumerateFiles ()) 

    let rec AllFilesIn name = // recursive walk of directory tree
        let files = Seq.singleton (FilesIn name)
        Seq.concat (Seq.append (Seq.map (fun d -> AllFilesIn d) (DirsIn name)) files)     

    // WRITER
    type Writer (fileName) =

        let mutable pos    = 1
        let mutable col    = 1
        let mutable spaces = 0
        let tw = File.CreateText fileName

        member w.Put (ch) = 
            if   ch = ' '
            then spaces <- spaces + 1
            else for i = 1 to spaces do tw.Write " "
                 tw.Write ch
                 pos <- pos + spaces + 1
                 spaces <- 0                 

        member w.NewLine () = 
            spaces <- 0
            pos <- 1
            col <- 1
            tw.WriteLine ()

        member w.EnsureLine ()=
            if pos <> 1 then w.NewLine ()

        member w.Tab () =
            if   col < 11
            then col <- 11
            else col <- (col + 3) / 10 * 10 + 7 // 1,11,17,27,37,47,...
            for i = 1 to max 1 (col-1-pos) do tw.Write " "
            spaces <- 0
            pos    <- col

        member w.Msg (s: string)  =
            for ch in s do w.Put ch

        member w.Header (buffer: byte[], bra, ket) =
            if   buffer.Length > 0
            then for bit in seq [1; 2; 4; 8; 18; 32; 64; 128] do
                     w.Msg bra
                     w.Msg " Legible Header "
                     for byte in buffer do
                        if  (int byte) &&& bit = bit
                        then w.Msg "O"
                        else w.Msg " "
                     w.Msg " "
                     w.Msg ket
                     w.NewLine ()

        member w.Close () =
            tw.Close ()
        
    let OddParity code =
           let rec Shift residual =
               if   residual = 0
               then 0
               else (residual &&& 1) + Shift (residual >>> 1)
           (Shift (code) &&& 1) = 1
        
    // READER
    // Extract successive characters from a byte array.  Yield -1 if reading past end

    type Reader (buffer: byte[]) = 
        
        let mutable ptr  = 0
        let mutable code = 0

        member s.ErrorFactor = min (buffer.Length/20) 5

        member s.GetPtr () = ptr
        member s.PutPtr p = ptr <- p
 
        member private s.Get () = // get next character
            code <- try (int buffer.[ptr]) with _ -> -1
            code
        
        member s.Scan (chars: byte[]) = // count remaining significant characters
             let mutable total = 0
             let mutable count = 0
             let mutable even  = 0
             let mutable eight = 0
             for i=ptr to buffer.Length-1 do
                let ch = buffer.[i]
                match ch with
                     | 0uy
                     | 255uy -> ()
                     |_      -> 
                                total <- total + 1
                                if ch &&& 128uy = 128uy then eight <- eight + 1
                                if not (OddParity (int ch)) 
                                then even <- even+1
                                     for c in chars do
                                        if   ch = c
                                        then count <- count+1
             let ft     = float total
             let fp     = float even
             let f8     = float eight
             let fc     = float count
             let evens  = fp / ft
             let eights = f8 / ft
             let oks    = fc / ft
             if   eights > 0.0 && evens > 0.90 // high ratio of even parity and some bit 8?
             then oks >= 0.10  // plausible ratio of white space
             else false

        member s.Read () = // get next character and advance pointer
            code <- s.Get ()
            ptr  <- ptr + 1
            code
        
        member s.Peek () = // look ahead one byte
            try (int buffer.[ptr + 1]) with _ -> -1

        member s.Reset () = // go back to start
            ptr  <- 0
            code <- 0

        member s.SkipBlanks () = // skip over blanks (0)
            if   s.Read () = 0
            then s.SkipBlanks ()
            else code

        member s.Header () = // look for legible header terminated by 20[+] blanks
                               // assumes current code and ptr are first candidate characters
                               // in header
            let start = ptr-1
            let rec SkipLegible code blanks threshold =
                if   code = -1
                then printfn "EOF"; (0, -1) // hit end of file
                elif ptr - start > threshold
                then (* printfn "THRESHOLD %d" threshold; *)(0, -1) // more than 1/4 of tape or 36"
                elif code = 0 
                then if    blanks < 20
                     then SkipLegible (s.Read ()) (blanks + 1) threshold // allow 1.5" of blanks in header
                     elif ptr-blanks-start < 15
                     then (* printfn "TOO SHORT AT %d" ptr; *) (0, -1) // less than 1.5" of header
                     else (* printfn "TERMINATING BLANKS AT %d" ptr; *) (start, ptr - blanks - 1) // run of blanks terminates
                elif code = 10
                then (* printfn "LINEFEED"; *) (0, -1) // bail if line feed 
                else SkipLegible (s.Read ()) 0  threshold        
            SkipLegible code 0  (min (buffer.Length/4) 360)              

        member s.Slice start stop =
            buffer.[start..stop]
    
    let SumCheck (b: byte[]) =
        let mutable (total: int64) = 0L
        for i = 0 to b.Length-1 do total <- total + (int64 b.[i])
        total

    let mutable parities  = 0
    let mutable haltcodes = 0
    let mutable problems  = 0

    let rec Decode900 (r: Reader, w: Writer, last, code, probs, pars) = 

        let teleCode900   = "¬¬¬¬¬¬¬¬¬\t"       + "\n¬¬\r¬¬¬¬¬¬" + "¬¬¬¬¬¬¬¬¬¬"    + "¬¬ !\"£$%&'" + 
                            "()*+,-./01"        + "23456789:;"   +  "<=>?@ABCDE"   + "FGHIJKLMNO"  + 
                            "PQRSTUVWXY"        + "Z[\\]^_`abc"  + "defghijklm"    + "nopqrstuvw"  + 
                            "xyz{|}‾¬"
                            
        let decode () =

            if   OddParity code
            then printfn "<! Parity Error (Code = %d) !>" code
                 w.Msg  (sprintf "<! %3d !>" code)
                 parities <- parities + 1
            else let s = code &&& 127
                 let ch = teleCode900.[s]

                 match code with
                 | 000   
                 | 255 ->    ()
                 | 135 ->    w.Msg "<! Bell !>"
                 | 020 ->    w.Msg "<! Halt !>"
                             haltcodes <- haltcodes + 1
                 | _   ->    match ch with
                             | '\r'   -> ()
                             | '\t'   -> w.Tab ()
                             | '\n'   -> w.NewLine ()
                             | '¬'    -> printfn "<! Unknown 900 character %03d !>" code
                                         w.Msg (sprintf "<! %d >!" code)
                                         problems <- problems + 1
                             | _      -> w.Put ch

        if   problems-probs > 10 || pars-parities > 10
        then printfn "<! file abandoned !>"
             () // bail on errors
        elif code = -1 //  stop at end of file
        then if last <> 20 
             then printfn "<! Final Character Not Halt Code !>"
                  problems <- problems + 1 
        else decode ()
             Decode900 (r, w, (if code = 0 then last else code), r.Read (), probs, pars)
 
    
    let Shuffle code = ((code >>> 1) &&& 0x70) ||| (code &&& 0x0f) // track 5 is parity in 920 telecode
    
    let rec Decode920 (r: Reader, w: Writer, last, code, header: byte[], probs, pars) = 
        
        if   header.Length > 0 then w.Header (header, "<!", "!>")

        let teleCode920   = "¬¬\n¬\t¬¬¬()" + ",£:&*/0123" + "456789⑩⑪=+"  + "-.;ABCDEFG" + "HIJKLMNOPQ" +
                            "RSTUVWXYZ¬"   + "¬¬|¬ ¬¬¬¬¬" + "¬¬¬¬¬¬¬¬¬¬"  + "¬¬¬¬¬¬¬¬[]" + "❿<>↑~%?abc" +
                            "defghijklm"   + "nopqrstuvw" + "xyz¬¬¬_¬" 

        let decode () = 

            if   OddParity code
            then printfn "<! Parity Error (Code = %d) !>" code
                 parities <- parities + 1
            else let s  = Shuffle code
                 let ch = teleCode920.[(int s)]

                 match s with
                 | 000 
                 | 127 ->    ()
                 | 076 ->    w.Msg "<! Halt !>"
                             haltcodes <- haltcodes + 1
                 | _   ->    match ch with
                             | '\t'   -> w.Tab ()            
                             | '\n'   -> w.NewLine ()
                             | '¬'    -> printfn "<! Unknown 920 character %03d !>" code 
                                         w.Msg (sprintf "<! %03d !>" code)
                                         problems <- problems + 1
                             | '|'    -> let p = teleCode920.[Shuffle (r.Read ())]
                                         match p with
                                         | '.'  -> w.Put '!'  // taken from T Froggat scantape utility
                                         | '~'  -> w.Put '"'
                                         | '='  -> w.Put '#'
                                         | 'S'  -> w.Put '$'
                                         | '<'  -> w.Put '\''
                                         | '2'  -> w.Put '½'  // TJF also uses this for ?
                                         | 'a'  -> w.Put '@'
                                         | '/'  -> w.Put '\\' 
                                         | '>'  -> w.Put '`'
                                         | '6'  -> w.Put '{'
                                         | '9'  -> w.Put '}'
                                         | ' '  -> w.Put '|'
                                         | '\t' -> w.Put '|'; w.Tab ()
                                         | '\n' -> w.Put '|'; w.NewLine ()
                                         | _    -> w.Msg (sprintf "|%c" p)
                                                   printfn "<! Unknown Pair '|' & '%c' !>" p
                                                   problems <- problems + 1
                             | '_'   ->  let p = teleCode920.[Shuffle (r.Read ())]
                                         match p with
                                         | ' '  -> w.Put '_'
                                         | '\t' -> w.Put '_'; w.Tab ()
                                         | '\n' -> w.Put '_'; w.NewLine ()
                                         | _    -> w.Msg (sprintf "|%c" p)
                                                   printfn "<! Unknown Pair '_' & '%c' !>" p
                                                   problems <- problems + 1
                             | _      -> w.Put ch

        if   problems-probs > 5 || pars-parities > 10
        then () // bail on errors
        elif code = -1 // stop at end of file
        then if last <> 156 
             then printfn "<! Final Character Not Halt Code !>"
                  problems <- problems + 1    
        else decode ()
             Decode920 (r, w, (if code = 0 then last else code), r.Read (), header, probs, pars) 

    let Trim (buffer: byte[]) = 
        let rec TrimLeft i =
            if   i >= buffer.Length
            then 0
            elif buffer.[i] = 0uy 
            then TrimLeft (i+1) 
            else i
        let rec TrimRight i =
            if   i <= 0
            then -1
            elif buffer.[i] = 0uy 
            then TrimRight (i-1) 
            else i
        let start  = TrimLeft 0
        let finish = TrimRight (buffer.Length-1)  
        buffer.[start..finish]

    let BinaryFile toPrefix (header: byte[]) (r: Reader) code  =
        
        let w = new Writer (toPrefix + ".BIN")
        w.Header (header, "(", ")")
        for j =1 to 2 do // two lines of run out
            for i=1 to 18 do w.Msg ("   0") // line of runout
            w.NewLine ()
        let mutable count = 0
        let mutable ch = code
        while ch <> -1 do
            w.Msg (sprintf "%4d" ch)
            count <- count + 1
            ch <- r.Read ()
            if   count >= 18
            then count <- 0
                 w.NewLine ()
        for i=1 to 18-count do w.Msg ("   0")
        w.NewLine ()
        for i= 1 to 18 do w.Msg ("   0")
        w.Close ()

    let mutable scanned = 0
    let mutable ignored = 0

    // Main Program

    let commandLine = System.Environment.GetCommandLineArgs();
    if  commandLine.Length < 3 
    then printfn "Command Line error: ScanTapes fromDir toDir"
    else let fromDir = (commandLine.[1]).Replace("/","\\")
         let toDir   = (commandLine.[2]).Replace("/","\\")
         if   not (File.Exists toDir)
         then System.IO.Directory.CreateDirectory toDir |> ignore
         printfn "<! Scanning From \"%s\" TO \"%s\" !>" fromDir toDir
         scanned   <- 0
         ignored   <- 0
         problems  <- 0
         parities  <- 0
         haltcodes <- 0
         try
            for fileName in AllFilesIn fromDir do 
                scanned <- scanned + 1
                printfn "<! File Name \" %s \" !>" fileName
                if   fileName.EndsWith ".920" || fileName.EndsWith ".900" || fileName.EndsWith ".903"
                     || fileName.EndsWith ".bin" 
                     || fileName.EndsWith ".2" || fileName.EndsWith ".3"
                     || fileName.EndsWith "_2" || fileName.EndsWith "_3"
                     || fileName.EndsWith "L2" || fileName.EndsWith "L3"
                     || fileName.EndsWith "U2" || fileName.EndsWith "U3"
                     || fileName.EndsWith ".DS_Store"
                     || fileName.EndsWith ".BAD"
                then () // skip over this file
                else let braw = File.ReadAllBytes fileName                            
                     if fileName.EndsWith ".1" || fileName.EndsWith "_1" || fileName.EndsWith "L1" || fileName.EndsWith "U1"
                     then // new source file group
                          let fromPrefix = fileName.Substring (0, fileName.Length-2)
                          let seperator = fileName.Substring (fileName.Length-2, 1)
                          // ensure all copies have same sum check
                          let s = SumCheck braw
                          for i = 2 to 3 do
                             let f = fromPrefix + seperator + i.ToString ()
                             if    File.Exists f
                             then if   SumCheck (Trim (File.ReadAllBytes fileName)) <> s
                                  then printfn "<! Sum Check Mismatch Between \"%s\" And \"%s\" !>" fileName f
                                       problems <- problems + 1
                          // form destination file name
                          // get name of file, less ".1"
                          let toPostfix = fromPrefix.Substring ((fromPrefix.IndexOf fromDir) + fromDir.Length)
                          // form name in target hierarchy
                          let toPrefix = toDir + toPostfix + (if seperator = "L" || seperator = "U" then "." + seperator else "")
                          let toDirName  = toPrefix.Substring (0, (toPrefix.LastIndexOf '\\'))
                          // create sub folder if required
                          if   toDirName <> "" && not (File.Exists toDirName)
                          then System.IO.Directory.CreateDirectory (toDirName) |> ignore
                          // copy source as raw binary
                          File.WriteAllBytes ((toPrefix + ".RAW"), braw)
                          // try to determine file type for translations 
                          let b = Trim braw  // strip back to content                                   
                          let r = new Reader (b)
                          let mutable code = r.SkipBlanks () 
                          let header =
                               let (start, stop) = r.Header ()
                               if   stop > start
                               then // Header found
                                    code <- r.SkipBlanks ()
                                    r.Slice start stop
                               else r.Reset ()
                                    code  <- r.SkipBlanks ()
                                    [||]
                          // printfn "-----AT %d CODE=%d" (r.Ptr()) code
                          // attempt to classify based on parity and white space
                          if   r.Scan ([|18uy; 20uy; 144uy; 156uy; |]) 
                          then printfn "<! %s Detected to be 920 telecode !>" toPrefix
                               let w = new Writer (toPrefix + ".920")
                               let prevProb = problems
                               let prevPar  = parities
                               Decode920 (r, w, code, code, header, problems, parities)                                
                               w.Close ()
                               if   problems-prevProb > r.ErrorFactor || parities > r.ErrorFactor
                               then problems <- prevProb; parities <- prevPar
                                    File.Delete (toPrefix+".920")
                                    printfn "<! %s Forced to be binary !>" toPrefix
                                    BinaryFile toPrefix header r code
                          elif r.Scan ([|141uy; 10uy; 9uy; 160uy; 34uy|])
                          then printfn "<! %s Detected to be 900 telecode !>" toPrefix
                               let w1 = new Writer (toPrefix + ".900")
                               if header.Length > 0 then w1.Header (header, "<!", "!>")        
                               let prevProb = problems
                               let prevPar  = parities
                               Decode900 (r, w1, code, code, problems, parities) 
                               w1.Close ()                               
                               if   problems-prevProb > r.ErrorFactor || parities-prevPar > r.ErrorFactor
                               then problems <- prevProb; parities <- prevPar
                                    File.Delete (toPrefix+".900")
                                    printfn "<! %s Forced to be binary !>" toPrefix
                                    BinaryFile toPrefix header r code
                          else printfn "<! %s Assumed to be binary !>" toPrefix
                               BinaryFile toPrefix header r code
                     else problems <- problems + 1
                          printfn "<! Ignored: \"%s\" File Name Extension Not Recognized !>" fileName
         with e -> printfn "<! Problem %s !>"  e.Message 
         printfn "\n<! %d Files Scanned %d Files Ignored !>"  scanned ignored
         printfn "<! %d Parity Errors %d Other problems !>" parities problems 
         printfn "FINISHED"
