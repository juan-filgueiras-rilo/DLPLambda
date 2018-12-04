(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(******************************************************************************
 *                                                                            
 * Module Support                                                             
 *                                                                            
 * See Module's Support interface support.mli for more info.                  
 *                                                                            
 *****************************************************************************)

open Format

module Error = struct

exception Exit of int

type info = FI of string * int * int | UNKNOWN
type 'a withinfo = {i: info; v: 'a}

let dummyinfo = UNKNOWN
let createInfo f l c = FI(f, l, c)

let errf f = 
  print_flush(); 
  open_vbox 0; 
  open_hvbox 0; f(); print_cut(); close_box(); print_newline(); print_newline();
  raise (Exit 1)

let printInfo =
  (* In the text of the book, file positions in error messages are replaced
     with the string "Error:" *)
  function
    FI(f,l,c) ->
      print_string f; 
      print_string ":"; 
      print_int l; print_string "."; 
      print_int c; print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; print_space(); f())

let err s = errf (fun()-> print_string "Error: "; print_string s; print_newline())

let error fi s = errfAt fi (fun()-> print_string s)

let error1 s =
  print_string "Error: "; print_string s;
  print_newline()

let error1At fi s =
  printInfo fi; print_string " Error: ";
  print_string s; print_newline(); print_newline()

end (* module error *)

(** ---------------------------------------------------------------------- **)
(* Some pervasive abbreviations -- opened everywhere by convention *)
module Pervasive = struct

type info = Error.info

let pr = Printf.printf

end (* module pervasive *)


