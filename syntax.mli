(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(** Module Syntax: syntax trees and associated support functions **)

open Support.Pervasive
open Support.Error

type _type =
    TpBool
  | TpNat
  | TpString
  | TpFloat
  | TpRecord of (string * _type) list
  | TpVar of int * int
  | TpApp of _type * _type

(** Data type definitions **)
type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmVar of info * int * int
  | TmAbs of info * string * _type * term
  | TmApp of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmString of info * string
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmLet of info * string * term * term
  | TmFix of info * term

type binding =
    NameBind 
  | TmAbbBind of (term option) * (_type option)(* 
  | TypeBind of _type*)

type command =
  | Eval of info * term
  | Bind of info * string * binding

(** Context **)
type context = (string * binding) list
val emptycontext : context
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val searchFromContextTerm : info -> context -> int -> term
val searchFromContextType : info -> context -> int -> _type

(** Shifting and substitution **)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> _type -> _type
val typeSubstTop: _type -> _type -> _type
val tptermSubstTop: _type -> term -> term

(** Printing **)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val prbinding : context -> binding -> unit
val printtype : context -> term -> (_type option) -> unit

(** Misc **)
val tmInfo: term -> info