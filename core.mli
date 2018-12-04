(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val debug : bool ref

val eval : context -> term -> term 
val evalbinding : context -> binding -> binding 
