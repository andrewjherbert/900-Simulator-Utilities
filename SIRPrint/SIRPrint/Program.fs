#light

// SIR Pretty Printer (based on 903 SIR assembler)
//
// SIRPrint file
//
// TO DO - adopt user tables of constants / pseudo instructions?
// TO DO - all versions of quotes for OBEYED INSTRUCTIONS
// TO DO - force output in 900 telecode 
// TO DO - MASIR directives, constants and literals

//
// Accepts input using visible characters of 920, 900 or 903 telecodes according to following conventions:
// acute:     ' ‘  |<
// grave:     `’ @ |>
// pound:     £ \
// dollar:    $    |S
// quote:     " ~
// uparrow:   ↑ ^ 
// backarrow: ← _ 
// N.B. no check is made to ensure use of a specific telecode is systematic.
//
// Assumed tab stops: 1,11,17,27,37,47,...
//
// Syntax
// 
// IDENTIFIER               Group of letters or digits commencing with a letter
// GLOBAL IDENTIFIER LIST   [  ] e.g., [CAT "DOG RAT]
// CONSTANTS
//  Integer or fraction     +|- e.g. -71032 -.5
//  Octal groups            & e.g. &770123
//  Alphanumeric groups     £ e.g. £A23
// ADDRESSES
//  Absolute                an unsigned integer
//  Block relative address  N;
//  S.I.R. relative         ;+|-N
//  Identified address      Identifier+|-Integer (if required)
//  Literal address         Any constant as above of = followed by a quasi instruction (absolute address only)
// INSTRUCTION              /f a
// WORD                     Constant or instruction
// SKIP                     >+N >N
// COMMENT/TITLE            (THIS IS A COMMENT) ((THIS IS A TITLE)
// PATCH                    ^A, ^LABEL or ^LABEL+|-Integer -- accepted as input but ignored
// RESTORE                  $ -- accepted as input but ignored
// TRIGGER                  <N    -- accepted as input but ignored
// OBEYED INSTRUCTION       `8 A' -- accepted as input but ignored
// OPTION                   *+N *N -- accepted as input but ignored
// END OF TAPE              Halt Code -- End of File acts as Halt Code
// END OF PROGRAM           %

open System.Text
open System.IO
          
module Assembler =

    open System.Collections.Generic

    // tab stops (first col = 0)

    let col2 = 10 // instruction
    let col3 = 16 // address
    let col4 = 26 // comment

    // global state
    let mutable buffer   = ""     // program to parse - always padded with terminating \n
    let mutable pos      = 0      // current position of scanner in program
    let mutable start    = 0      // start of current line
    let mutable blocks   = 1      // block count
    let mutable blank    = true   // true if last line was blank
    let mutable errors   = false  // set true on an error
    
    // Output
    let mutable outpos  = -1       // last column filled
       
    let BlankLine ()    = 
        stdout.WriteLine ()
        outpos <- -1

    let StartNewLine () =
        if   outpos <> -1
        then BlankLine ()

    let rec Output (s: string)    =
        blank <- false
        if outpos+s.Length > 79
        then BlankLine ()
             Output s
        else stdout.Write s
             outpos <- outpos+s.Length

    let rec Tab pos         =
        if pos < outpos
        then BlankLine ()
             Tab pos
        elif pos > 79
        then failwith "Line Overflow!"
        else //
             if   outpos+1 < pos-1 
             then for i = outpos+1 to pos-1 do stdout.Write ' '
                  outpos <- pos-1  
             else stdout.Write ' '
                  outpos <- outpos+1         
    
    // Error handling - skip to newline       
    let rec Error () =
        errors <- true
        let Helper () =
           // from pos, find a seperator
           if   pos < buffer.Length && (buffer.[pos] <> '\n')
           then pos <- pos+1; 
                Error () // keep looking
        Helper ()
        BlankLine ()
        Output "*** Abandoned due to errors in SIR ***"

    // Character set: allow     ‘'  for acute
    //                          `’@ for grave
    //                          £\ for pound
    //                          ❿?  for subscript ten
    //                          ↑^  for up arrow
    //                          ←_  for back arrow
    let Code (ch: char) =
        let translate ch = 
            match ch with  // map 900 and 920 telecodes into 903 telecode subset used by SIR
            | '‘'  -> '\''
            | '’'
            | '@'  -> '`'
            | '\\' -> '£'
            | '❿'  -> 'º'
            | '?'  -> 'º'
            | '^'  -> '↑'
            | '_'  -> '←'
            | '~'  -> '"'
            | _    -> ch
        // 920 escapes, such as |<, handled locally in alphanumerics and titles
        let SIRInternal = " \n\"½$%&'()*+,-./0123456789:;<=>❿`ABCDEFGHIJKLMNOPQRSTUVWXYZ[£]↑^←_"
        SIRInternal.IndexOf ch

    let Digit  ch =  '0' <= ch && ch <= '9'

    let Letter ch = ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z')

    let Separator (ch: char) = (" \t\n".IndexOf ch) <> -1

    let Valid ch = (Code ch) <> -1

    // Scanning 

    exception EOF

    let NewLine () =
        if   pos < buffer.Length-1 // check not at final sentinel \n
        then pos <- pos+1
        else raise EOF

    let ScanDigits () =
        // from pos, count number of digits
        let rec ScanDigitsHelper count =
            let ch = buffer.[pos]
            if   '0' <= ch && ch <= '9'
            then  pos <- pos+1; ScanDigitsHelper (count+1)
            else  count
        ScanDigitsHelper 0

    let ReadIdentifier (prefix: string) =
        // Read identifier
        // buffer.[pos] = first letter
        let start = pos
        pos <- pos+1
        let rec ScanIdentifierHelper count =
            let ch = buffer.[pos]
            if   (Letter ch) || (Digit ch)
            then pos <- pos+1; ScanIdentifierHelper (count+1)
            else count
        let count = ScanIdentifierHelper 1
        if outpos+count+prefix.Length > 79
        then BlankLine ()
             if prefix <> "" then Output " "
        Output prefix
        Output ((buffer.Substring (start, count)).ToUpper ()) 

    let SkipBlanks () =
        // From pos, skip over spaces and tabs, return terminating ch
        let rec SkipBlanksHelper count = 
            let ch = buffer.[pos]
            if   ch = ' ' || ch = '\t' || ch = '\r'
            then pos <- pos+1; SkipBlanksHelper (count+1)
            else ch
        SkipBlanksHelper 0

    // Lexical items

    let Absolute () =
        // Read an absolute address N
        // buffer.[pos] is initial digit
        pos <- pos+1 // step over digit
        let digits = ScanDigits () + 1
        let str = buffer.Substring (pos-digits, digits)
        Output str

    let Alphanumeric () =
        // Read an alphanumeric group, e.g., £A23
        // buffer.[pos] is £
        let result = new StringBuilder ()
        result.Append buffer.[pos] |> ignore
        let rec AlphaHelper count =  
            let ch = buffer.[pos]                                  
            if count = 3 
            then () // all read
            else match ch with 
                 | '\t'    // tab treated as space
                 | ' '  -> result.Append " " |> ignore
                           pos <- pos+1
                           AlphaHelper (count+1) // space is 0 
                 | '\r'
                 | '\n' -> // return or newline terminates
                           ()
                 | '↑'  
                 | '^'  -> // ↑ decodes as newline (01)
                           result.Append ch |> ignore
                           pos <- pos+1; AlphaHelper (count+1)
                 | '|'  -> //920 escape
                           pos <- pos+1
                           let ch = buffer.[pos]
                           let code =
                                match ch with
                                | '<'   -> result.Append "|<" |> ignore
                                | '>'   -> result.Append "|>" |> ignore
                                | 'S'   -> result.Append "|S" |> ignore
                                | _     -> Error ()
                           pos <- pos+1
                           if pos >= buffer.Length then Error ()
                           AlphaHelper (count+1)
                 | _    -> pos <- pos+1
                           result.Append ch |> ignore
                           AlphaHelper (count+1)  
        pos <- pos+1 // step over £               
        AlphaHelper 0
        Output (result.ToString())

    let Fraction () =
        // Read a fraction +.N, -.N
        // buffer.[pos-1] = + | -
        // buffer.[pos  ] = .
        pos <- pos+1 // advance to first digit
        let digits = ScanDigits ()
        Output (buffer.Substring (pos-digits-2, digits+2))

    let Integer () =
        // Read an integer +N, -N
        // buffer.[pos] = sign + | -
        pos <- pos+1 // step over sign
        let digits = ScanDigits ()
        Output (buffer.Substring (pos-digits-1, digits+1))

    let IntegerOrFraction () =
        // Read a decimal integer or fraction +N -N, +.N, -.N
        // buffer.[pos] is sign + | -
        pos <- pos+1 // step over sign
        if buffer.[pos]  = '.'
        then Fraction () 
        else pos <- pos-1
             Integer ()     

    let Octal () =
        // read an octal group e.g., &770123
        // buffer.[pos] is &
        let rec OctalHelper count =
            let ch = buffer.[pos]
            if   '0' <= ch && ch <= '7'
            then pos <- pos+1; OctalHelper (count+1)
            else count
        pos <- pos+1 // step over &
        let count = OctalHelper 0 
        Output buffer.[pos-count-1..pos-1]

    // Addresses
    let Literal f =
        // Literal can be alphanumeric, fraction, integer, octal or quasi instruction
        f ()

    let QuasiInstruction () =
        // read a quasi instruction, e.g., =/5 N
        // buffer.[pos] is =
        // look for modify
        Output "="
        pos <- pos+1
        let ch = buffer.[pos]
        // unpicked function code and modifier
        if   ch = '/'
        then pos <- pos+1
             Output "/"
        if   not (Digit buffer.[pos])
        then Error ()  // no function after /
        else Absolute () // function
             let ch = SkipBlanks ()
             Output " "
             // unpick address
             if   not (Digit ch) 
             then  Error () // not absolute
             else Absolute ()      

    let Address () =
        // Read the address field of an instruction
        // buffer.[pos] is first character
        let ch = buffer.[pos] 
        match ch with
        | _ when Digit ch   -> // absolute or block relative
                            Absolute ()
                            // Look to see if block relative
                            let ch = buffer.[pos]
                            if   ch = ';' 
                            then // block relative
                                 pos <- pos+1
                                 Output ";" 

        | _ when Letter ch  -> // identifier+|-N 
                            let id = ReadIdentifier "" 
                            stdout.Write id                             
                            let ch = buffer.[pos]
                            if   ch = '+' || ch = '-'
                            then // identifier+|-offset form
                                 Integer () 

        | ';'              ->  // relative ;+|-N
                            pos <- pos+1
                            let ch = buffer.[pos]
                            if   ch = '+' || ch = '-'
                            then Output ";"
                                 Integer ()
                            else Error () // no digit after +|-

        | '+'               
        | '-'              ->  Literal IntegerOrFraction  // integer or fraction literal                            
        | '&'              ->  Literal Octal              // octal group literal
        | '£'              
        | '\\'             ->  Literal Alphanumeric       // alphanumeric group literal
        | '='              ->  Literal QuasiInstruction   // quasi instruction literal
        | _                ->  Error ()                   // unexpected character

    // Directives
    let Label () =
        // read a label ID or ID = N
        // buffer.[pos] is initial letter
        // read identifier
        StartNewLine ()
        ReadIdentifier ""
        if    buffer.[pos] = '='
        then // Id=A form
             pos <- pos+1
             if   Digit buffer.[pos]
             then Output "="
                  Absolute () |> ignore

    let Instruction ()  =
        // read an instruction F N
        // extract function F
        // buffer.[pos] = first digit of F
        Absolute ()
        SkipBlanks () |> ignore
        Tab col3
        Address ()

    let ModifiedInstruction quasi =
        // read a modified instruction /F N
        Output "/"
        pos <- pos+1
        if   Digit buffer.[pos]
        then Instruction ()
        else Error ()

    let GlobalIdentifierList () =
        // read a GlobalIdentifierList
        // buffer.[pos] = [
        StartNewLine ()
        Output "["
        blocks <- blocks + 1
        pos <- pos+1
        let rec NextId () =
            let ch = buffer.[pos]
            match ch with
            | ' '
            | '\t'                 
            | '\r' 
            | '\n'                -> // look for next identifier
                                      pos <- pos+1; NextId () // look for next identifier
            | ']'                  -> // end of list
                                      pos <- pos+2
                                      if outpos >= 79
                                      then StartNewLine ()
                                           Output "]"
                                      else Output " ]"
            | '"'                  
            | '~'                  -> // sub global
                                      pos <- pos+1
                                      if   Letter buffer.[pos]
                                      then ReadIdentifier (" " + (ch.ToString())) 
                                           NextId ()
                                      else Error ()
            | _ when Letter (ch)   -> ReadIdentifier " "
                                      NextId ()
            | _                    -> Error () // symbol not expected
        NextId ()

    let Comment () =
        // read a comment (....)
        // buffer.[pos] is (
        // comments at start of a line get a blank before them
        // try to preserve layout in comments
        let rec CommentHelper tab start max =
            let ch = buffer.[pos]
            if   ch = ')'
            then pos<-pos+1
                 Output ")"
            elif ch = '\n'
            then // can output this segment
                 StartNewLine ()
                 Tab tab
                 pos <- pos+1
                 CommentHelper tab pos max
            elif pos-start > max
            then // have to break the line
                 StartNewLine ()
                 Tab tab
                 pos<-pos+1
                 CommentHelper tab pos max
            else // Output the character
                 if    ch <> '\r'
                 then Output (ch.ToString())
                 pos<-pos+1
                 CommentHelper tab pos max
        if   blank 
        then StartNewLine ()
             CommentHelper 0 pos 80   
        else Tab col4
             CommentHelper col4 pos 80                 

    let Constant f  =
        // read a numeric constant (integer or fraction) +N -N, +.N, -.N, 
        // or Octal &477777 or Alphanumeric £ABC
        // buffer.[pos] is one of +, -, &, £
        Tab col2
        f () // read in constant group
                
    let Skip () = 
        // read a skip directive >+N
        // buffer.[pos] is >
        if outpos > col2 then StartNewLine ()
        Tab col2
        Output ">"
        pos <- pos+1
        if   buffer.[pos] = '+' // optional + after >
        then pos <- pos+1
        if not (Digit buffer.[pos])
        then Error ()
        else Absolute ()

    let Obey () =
        // read an obeyed instruction '8 N`
        // buf.[pos] is ' or equivalent
        StartNewLine ()
        Tab col2
        let Helper i = 
            i // get instruction
            let ch = SkipBlanks () 
            match ch with
            | '`'   ->  Output "`"
            | '@'   ->  Output "@"
            | '’'   ->  Output "’"
            | _     ->  Error ()
        pos <- pos+1
        let ch = SkipBlanks ()
        if   ch = '/'
        then Helper (ModifiedInstruction true)
        elif Digit ch 
        then Helper (Instruction ())
        else Error ()

    let Option () =
        // read an option directive *N or *+N
        // buf.[pos] is *
        StartNewLine ()
        Output "*"
        pos <- pos+1
        let ch1 = buffer.[pos]
        if ch1 = '+'
        then pos <- pos+1
        let ch2 = buffer.[pos]
        if   Digit ch2
        then // ^N 
             Absolute ()
        else Error () // * not followed by digits
             
    let Patch () =
        // read a patch directive ^A
        // buffer.[pos] is ^
        StartNewLine ()
        Output "^"
        pos <- pos+1
        let ch = buffer.[pos]
        if ch = '+' then pos <- pos + 1
        let ch = buffer.[pos]
        if ch = '"' || ch = '~'
        then Output "\""
             pos <- pos+1
        if   Digit buffer.[pos]
        then // ^N 
             Absolute ()
        elif Letter ch
        then // ^L, ^L+N, ^L-N, where L is a located label
             ReadIdentifier ""
             let ch = buffer.[pos] 
             if   ch = '+' || ch = '-'
             then if    not (Digit buffer.[pos+1])
                  then Error ()
                  else Integer ()

    let Restore () =
        // read a restore directive $
        StartNewLine ()
        Output "$"
        pos <- pos+1

    let Terminate () =
        // read a terminate directive %
        StartNewLine ()
        Output "%"
        pos <- pos+1

    let Trigger () =
        // read a trigger directive >
        pos <- pos+1
        Output (if pos=0 || buffer.[pos-1]='\n' then "<" else " <")
        if SkipBlanks () <> '\n' then Output " "

    let Halt () =
        // read a halt code escape sequence
        StartNewLine ()
        Output "<! Halt !>"
        StartNewLine ()

    let TriggerOrHalt () =
        // unpick <
        if   buffer.[pos+1] <> '!'
        then Trigger ()
        elif ( let prefix = buffer.[pos..pos+9] 
               prefix.ToUpper () = "<! HALT !>" )
        then pos <- pos+10
             Halt ()
        else Error ()

    // Parser
    let rec ParseSymbols () = // Determine type of next symbol in buffer  
        if   (not errors) && pos < (buffer.Length-1) // don't parse sentinel \n at end of buffer
        then let ch = buffer.[pos]
             let sp = ' '
             // printfn "ParseSymbols @ buffer.[%d] = \"%s\"" pos buffer.[pos..(min (pos+10) (buffer.Length-1))]
             match ch with
             | ' '
             | '\r'
             | '\t' ->  pos<-pos+1 
             | '\n' ->  blank <- true
                        NewLine ()
                        BlankLine () // preserve newlines
             | _    ->  match ch with                         
                        | _ when Letter (ch) 
                                        // Label
                                    ->  Label () 
                        | _ when Digit (ch) 
                                        // Instruction
                                    ->  Tab col2
                                        Instruction ()                              
                        | '*'       ->  Option ()
                        | '/'       ->  // Modified instruction
                                        Tab col2
                                        ModifiedInstruction false
                        | '['       ->  // Global identifier list
                                        GlobalIdentifierList ()
                        | '+'         
                        | '-'       ->  // Integer or Fraction constant
                                        Constant IntegerOrFraction
                        | '&'       ->  // Octal constant
                                        Constant Octal                               
                        | '£'       
                        | '\\'      ->  // Alphanumeric constant
                                        Constant Alphanumeric 
                        | '>'       ->  // Skip memory
                                        Skip ()
                        | '<'       ->  // Trigger
                                        TriggerOrHalt ()  
                        | '('       ->  // Comment
                                        Comment ()  
                        | '^'       ->  // Patch
                                        Patch ()    
                        | '$'       ->  // Restore
                                        Restore ()  
                        | '%'       ->  // Terminate
                                        Terminate ()    
                        | '*'       ->  // Option
                                        Option ()    
                        | '\''      
                        | '‘'       ->  // Obeyed instruction
                                        Obey ()                                        
                        | _         ->  Error ()   
             ParseSymbols ()

    let Assembler (program: string) = 
        // ensure program ends with a newline
        buffer <- if program.EndsWith "\n" then program else program+"\n"
        // printfn "%s" buffer
        ParseSymbols () 

    // MAIN PROGRAM
    let args = System.Environment.GetCommandLineArgs ()
    if args.Length < 2
    then failwith "No input file specified"
    let program = 
        try File.ReadAllText (args.[1]) with
        | err ->  failwith err.Message
    if    args.Length > 2
    then let out = new StreamWriter (args.[2])
         System.Console.SetOut out
    Assembler program
    // System.Console.ReadLine () |> ignore
    stdout.Close ()
