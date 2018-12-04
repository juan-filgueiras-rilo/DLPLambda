(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{
open Support.Error

  (* reservedWords are special words or symbols with functional meaning, they can't be used as variable names *)
let reservedWords = [
  (* Keywords *)
  ("if", fun i -> Parser.IF i);
  ("then", fun i -> Parser.THEN i);
  ("else", fun i -> Parser.ELSE i);
  ("true", fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("lambda", fun i -> Parser.LAMBDA i);
  ("timesfloat", fun i -> Parser.TIMESFLOAT i);
  ("succ", fun i -> Parser.SUCC i);
  ("pred", fun i -> Parser.PRED i);
  ("iszero", fun i -> Parser.ISZERO i);
  ("let", fun i -> Parser.LET i);
  ("in", fun i -> Parser.IN i);
  
  (* Symbols *)
  ("_", fun i -> Parser.USCORE i);
  ("'", fun i -> Parser.APOSTROPHE i);
  ("\"", fun i -> Parser.DQUOTE i);
  ("!", fun i -> Parser.BANG i);
  ("#", fun i -> Parser.HASH i);
  ("$", fun i -> Parser.TRIANGLE i);
  ("*", fun i -> Parser.STAR i);
  ("|", fun i -> Parser.VBAR i);
  (".", fun i -> Parser.DOT i);
  (";", fun i -> Parser.SEMI i);
  (",", fun i -> Parser.COMMA i);
  ("/", fun i -> Parser.SLASH i);
  (":", fun i -> Parser.COLON i);
  ("::", fun i -> Parser.COLONCOLON i);
  ("=", fun i -> Parser.EQ i);
  ("==", fun i -> Parser.EQEQ i);
  ("[", fun i -> Parser.LSQUARE i); 
  ("<", fun i -> Parser.LT i);
  ("{", fun i -> Parser.LCURLY i); 
  ("(", fun i -> Parser.LPAREN i); 
  ("<-", fun i -> Parser.LEFTARROW i); 
  ("{|", fun i -> Parser.LCURLYBAR i); 
  ("[|", fun i -> Parser.LSQUAREBAR i); 
  ("}", fun i -> Parser.RCURLY i);
  (")", fun i -> Parser.RPAREN i);
  ("]", fun i -> Parser.RSQUARE i);
  (">", fun i -> Parser.GT i);
  ("|}", fun i -> Parser.BARRCURLY i);
  ("|>", fun i -> Parser.BARGT i);
  ("|]", fun i -> Parser.BARRSQUARE i);

  (* Special compound symbols: *)
  (":=", fun i -> Parser.COLONEQ i);
  ("->", fun i -> Parser.ARROW i);
  ("=>", fun i -> Parser.DARROW i);
  ("==>", fun i -> Parser.DDARROW i);
]

(* Support functions *)


type buildfun = info -> Parser.token

(* This creates a hashtable for the reservedWords *)
let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024

(* All reservedWords are added to the hashtable *)
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

(* Tries to find str in the hashtable *)
(* If it finds it, it is returned as a token with its info *)
(* if it can't find it, it creates a new upper or lowercase ID (a variable) with location info and the value of str *)
let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
       Parser.UCID {i=i;v=str}
    else 
       Parser.LCID {i=i;v=str}

(* Initialisation of all variables *)

(* Lineno is the line number *)
let lineno   = ref 1
(* Depth accounts for nested comment depth *)
and depth    = ref 0
(* Start stores first character location value *)
and start    = ref 0
(* This stores the name and location of the file being parsed in a reference *)
and filename = ref ""
(* Startlex stores information on where lexbuf was located when entering any comment or string literal *)
and startLex = ref dummyinfo

(* create opens a file either by its name or its location + name *)
let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

(* Every new line, this increments the linenumber and sets start to the first character in that line *)
let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

(* When location info should be appended, this quotes the filename, line number and location *)
let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

(* Returns the current matched string *)
let text = Lexing.lexeme

(* Stores the string being parsed with literal string rules *)
let stringBuffer = ref (String.create 2048)

(* Tracks where the string contained in stringbuffer ends *)
let stringEnd = ref 0

(* Resets string end back to the beginning *)
let resetStr () = stringEnd := 0

(* addStr adds a new character to the stringBuffer and increments stringEnd *)
(* This is used when parsing literal strings *)
(* If stringBuffer is too small, a newBuffer of twice the size is created and used instead as stringBuffer *)
let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = String.length buffer then
    begin
      let newBuffer = String.create (x*2) in
      String.blit buffer 0 newBuffer 0 x;
      String.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      String.set buffer x ch;
      stringEnd := x+1
    end

(* getStr returns the string contained in the buffer using stringEnd as length *)
let getStr () = String.sub (!stringBuffer) 0 (!stringEnd)

(* Extracts a line number from text, used with "# " and "# line" *)
let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))
}


(* The main body of the lexical analyzer *)

rule main = parse
  (* Tab or formfeed are ignored *)
  [' ' '\009' '\012']+     { main lexbuf }
  (* Newline calls newline function *)
| [' ' '\009' '\012']*("\r")?"\n" { newline lexbuf; main lexbuf }
  (* If it finds a comment end without being in a comment, it calls an error *)
| "*/" { error (info lexbuf) "Unmatched end of comment" }
  (* A comment start calls comment rules into action, and stores start information, main is called afterwards *)
| "/*" { depth := 1; startLex := info lexbuf; comment lexbuf; main lexbuf }
  (* Updates lineno and calls getFile rules *)
| "# " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 2 - 1; getFile lexbuf }
  (* Same as above but discarding "line " *)
| "# line " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 7 - 1; getFile lexbuf }
  (* An integer is identified as INTV, its location info and value stored (uses text, or Lexing.lexeme) *)
| ['0'-'9']+
    { Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} }
  (* A float is identified as FLOATV, its location info and value stored (uses text, or Lexing.lexeme) *)
| ['0'-'9']+ '.' ['0'-'9']+
    { Parser.FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} }
  (* Any "word" is passed through createID *)
| ['A'-'Z' 'a'-'z' '_']
  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { createID (info lexbuf) (text lexbuf) }
  (* Any reserved symbol is passed through createID too *)  
| ":=" | "<:" | "<-" | "->" | "=>" | "==>"
| "{|" | "|}" | "<|" | "|>" | "[|" | "|]" | "=="
    { createID (info lexbuf) (text lexbuf) }
  (* Any oher known symbols pass through createID too *) 
| ['~' '%' '\\' '+' '-' '&' '|' ':' '@' '`' '$']+
    { createID (info lexbuf) (text lexbuf) }
  (* Same as above *)  
| ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ','
   '=' '\'']
    { createID (info lexbuf) (text lexbuf) }
  (* Quotation mark calls for literal string parsing *)
| "\"" { resetStr(); startLex := info lexbuf; string lexbuf }
  (* EOF calls for end of file *)
| eof { Parser.EOF(info lexbuf) }
  (* Any other unrecognised character should call an error *)
| _  { error (info lexbuf) "Illegal character" }
  
(* The comment rules of the lexical analyser *)
(* This is called whenever a /* is found *)
and comment = parse
  (* Nested comments mean depth should be increased and comment parsed *)
  "/*"
    { depth := succ !depth; comment lexbuf }
  (* Closing a comment returns and decreases depth *)
  (* If there is still any depth, it keeps using comment rules *)
| "*/"
    { depth := pred !depth; if !depth > 0 then comment lexbuf }
  (* Reaching eof inside a comment means it is not closed *)  
| eof
    { error (!startLex) "Comment not terminated" }
  (* Anything other than newline is ignored as comment *)  
| [^ '\n']
    { comment lexbuf }
  (* Newline means we should increase lineno *)  
| "\n"
    { newline lexbuf; comment lexbuf }

(* GetFile rules: After a line reference, white spaces are ignored *)
(* until a double quote is found and getName rules are called *)    
and getFile = parse
  " "* "\"" { getName lexbuf }

(* GetName rules: Anything that is not "" or newline is treated as part of the filename *) 
(* This updates the filename reference and calls finishname rules.*)
and getName = parse
  [^ '"' '\n']+ { filename := (text lexbuf); finishName lexbuf }

(* FinishName rules: when a closing double quote is found, parsing continues using main rules *) 
and finishName = parse
  '"' [^ '\n']* { main lexbuf }

(* String rules parse literal strings *)
and string = parse
  (* A second quotation mark indicates string ends *)
  '"'  { Parser.STRINGV {i = !startLex; v=getStr()} }
  (* Any escape character must be parsed by escaped rules *)
| '\\' { addStr(escaped lexbuf); string lexbuf }
  (* Any newline is added to the parsed string but lineno is increased *)
| '\n' { addStr '\n'; newline lexbuf; string lexbuf }
  (* Reaching eof inside a string definition calls an error *)
| eof  { error (!startLex) "String not terminated" }
  (* Any other character is added to the string *)
| _    { addStr (Lexing.lexeme_char lexbuf 0); string lexbuf }

(* Escaped rules parse escape characters *)
and escaped = parse
  (* Newline *)
  'n'	 { '\n' }
  (* Tab *)
| 't'	 { '\t' }
  (* Backslash *)
| '\\'	 { '\\' }
  (* Double quote *)
| '"'    { '\034'  }
  (* Single quote *)
| '\''	 { '\'' }
  (* Any ASCII character is parsed to its value unless it's not an ASCII character*)
| ['0'-'9']['0'-'9']['0'-'9']
    {
      let x = int_of_string(text lexbuf) in
      if x > 255 then
	error (info lexbuf) "Illegal character constant"
      else
	Char.chr x
    }
  (* Anything other than that calls for an error *)  
| [^ '"' '\\' 't' 'n' '\'']
    { error (info lexbuf) "Illegal character constant" }

(*  *)
