(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(*******************************************************************************
 *                                                                             
 * Module Toplevel	                                                       
 *                                                                             
 * toplevel.ml manages the command line and executes the parsed commands.      
 *                                                                             
 * These are the available directives. They must be followed by a semicolon.   
 * 	- #var		:	List bound identifiers                         
 *	- #debug	:	Toggles debug mode to show intermediate        
 *                              evaluations        		               
 *      - #quit, #exit	:	Finalises program execution                    
 *	                                                                       
 *                                                                             
 ******************************************************************************)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

(* the toplevel directives to exit the program *)
let exit_program = ["#quit;";"#exit;"]

(* identifiers list *)
let identifiers = ref []

(* update_identifiers updates the identifiers list. If symbol does not exist, 
   the identifier is added with its value. Otherwise, its value is overriden *)
let update_identifiers symbol value =
  let rec inner_update rest bindings = match bindings with
    [] -> identifiers := List.rev ((symbol,value)::rest)
    | (s,v)::tl -> if s = symbol then identifiers := List.append (List.rev rest) ((s,value)::tl)
                                 else inner_update ((s,v)::rest) tl 
  in inner_update [] !identifiers

(* parseLine receives the line to parse and creates a Lexing.lexbuf to be scanned.
   A function that takes a context and returns a list of commands and said context
   as pair is returned. If any error ocurrs, said list is returned empty *)
let parseLine line =
  let lexbuf = Lexing.from_string line
  in let result =
    try Parser.toplevel Lexer.main lexbuf 
    with Parsing.Parse_error -> error1At (Lexer.info lexbuf) "Parse error"; (fun ctx -> [],ctx)
in
  result

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | TmAbbBind(Some(t),None) -> TmAbbBind(Some(t), Some(gettype ctx t))
  | TmAbbBind(t,Some(tyT)) -> TmAbbBind(t,Some(tyT))
  | TmAbbBind(None,None) -> TmAbbBind(None,None)

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

(* called by the toplevel each time it reads user input data. The line is parsed, a
   list of commands is generated, executed and printed. A new context is returned.
   If an error occurs while parsing, Exit exception is catched and same context is
   returned. *)
let process_line line ctx =
  try
    let cmds,_ = parseLine line ctx in
    let g ctx c =  
      open_hvbox 0;
      let results = process_command ctx c in
      print_flush();
      results  
    in 
      List.fold_left g ctx cmds
  with Exit _ -> ctx

(* to show the identifiers *)
let print_identifiers ctx =
  let rec pr_bind binding = match binding with
    [] -> ()
    | (x,bind)::rest -> if rest <> [] then begin
					pr "%s, " x; pr_bind rest
				      end else begin 
					pr "%s\n" x;
				      end
  in pr_bind !identifiers

(* the top level main method. Prints the prompt and waits for user input. The line read 
   and input ctx are passed to process_line; results are printed and a new context is generated *)
let rec top_level ctx = 
  pr ">> "; 		
  let line = read_line () in
    if List.exists (fun x -> x = line) exit_program then raise (Exit 0)
    else
      if line = "#var;" then begin print_identifiers ctx; top_level ctx end
      else if line = "#debug;" then 
        begin 
          debug := not !debug; 
          if !debug then pr ":Debugging mode activated\n" 
          else pr ":Debugging mode deactivated\n"; top_level ctx 
        end else let ctx' = process_line line ctx in top_level ctx' 
