(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(******************************************************************************
 *                                                                            
 * Module Main	                                                              
 *                                                                            
 * The main program. Deals with processing the command line, reading files,   
 * building and connecting lexers and parsers, etc. For most experiments with 
 * the implementation, it should not be necessary to change this file.        
 *                                                                            
 *****************************************************************************)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core
open Toplevel


(* searchpath will contain the directories specified by the user where the program 
   will try to open the file. The empty string represents the local path *)
let searchpath = ref [""]

(* Used by Arg.parse. '-I' is the only keyword recognized by the command line parser.
   When found, the directory that follows is appended to searchpath *)
let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

(* parseArgs reads the command line and returns the name of file specified by the user 
   or the string '{toplevel}' in case no file was specified to start the top level *)
let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must either specify exactly one input file or none to 
			 start the top level"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> "{toplevel}"
    | Some(s) -> s

(* openfile tries to create an inputstream (in_channel) on the file introduced by the user 
   searching on the directories that searchpath contains *)
let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath


(* parseFile creates a Lexing.lexbuf from the input file and passes it to the parser with
   Lexer.main to create a list of commands to be executed. It returns a pair which first
   element is the list and the second a context. If an error occurs while parsing a parsing
   exception is raised and execution ends *)
let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf 
    with Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

(* name of files that have been already imported *)
let alreadyImported = ref ([] : string list)
  
(* processes the given command. Used by both methods process_file and process_line.
   Results are printed a new context is returned *)
let rec process_command ctx cmd = match cmd with
  | Eval(fi,t) ->
      let tp = gettype ctx t in
      let t' = eval ctx t in
      printtm_ATerm true ctx t'; 
      force_newline();
      pr ": ";
      printtype ctx t (Some(tp));
      force_newline();
      ctx
  | Bind(fi,x,bind) -> 
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind in
      pr "%s" x; pr " "; prbinding ctx bind'; (* prbindingtype ctx bind';*)force_newline(); 
      update_identifiers x bind';
      addbinding ctx x bind'

(* called if a file is specified to be processed. The file is parsed, a list of commands 
   is created and executed*)
let process_file f ctx =
  alreadyImported := f :: !alreadyImported;
  let cmds,_ = parseFile f ctx in
  let g ctx c =  
    open_hvbox 0;
    let results = process_command ctx c in
    print_flush();
    results
  in
    List.fold_left g ctx cmds

(* main calls process_file to process the file introduced by the user. If no file was
   specified, the top-level is started *)
let main () =
  let mode = parseArgs() in
    if mode <> "{toplevel}" then let inFile = mode in
      let _ = process_file inFile emptycontext in
        ()
    else
      let _ = top_level emptycontext in 
      	() 

(* pretty-printing functions. See Format module in the OCaml library for more info *)
let () = set_max_boxes 1000
let () = set_margin 67

(* where main method is called. At the end of the execution res will take the value of 
   an int which will be used as an exit code to exit the program *)
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()

(* To reset the pretty-printer to initial state *)
let () = print_flush()
(* To finalize the process with the given status *)
let () = exit res
