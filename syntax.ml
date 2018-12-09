(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(*****************************************************************************
 *                                                                           
 * Module Syntax                                                             
 *                                                                           
 * Syntax trees and associated support functions                             
 *                                                                           
 ****************************************************************************)

open Format
open Support.Error
open Support.Pervasive

(** ---------------------------------------------------------------------- **)
(** Datatypes **)

(* Types recognized by the program *)
type _type =
    TpBool
  | TpNat
  | TpApp of _type * _type

(* Terms recognized by the program *)
type term =
    TmTrue of info					(* Boolean True *)
  | TmFalse of info					(* Boolean False *)
  | TmIf of info * term * term * term			(* If/then/else *)
  | TmVar of info * int * int				(* Variable *)
  | TmAbs of info * string * _type * term			(* Abstraction *)
  | TmApp of info * term * term				(* Application *)
  | TmRecord of info * (string * term) list		(* Record *)
  | TmProj of info * term * string			(* Projection *)
  | TmFloat of info * float				(* Float *)
  | TmTimesfloat of info * term * term			(* Product of floats *)
  | TmString of info * string				(* String *)
  | TmZero of info					(* Zero *)
  | TmSucc of info * term				(* Successor *)
  | TmPred of info * term				(* Predecessor *)
  | TmIsZero of info * term				(* IsZero *)
  | TmLet of info * string * term * term		(* Local variable *)

(* 2 types of binding. The standalone and the one associated with a term *)
type binding =
    NameBind 
  | TmAbbBind of term * (_type option)

(* The context type, a list of bindings and its symbols *)
type context = (string * binding) list

(* 2 types of commands to be executed: evaluations and bindings *)
type command =
  | Eval of info * term
  | Bind of info * string * binding

(** ---------------------------------------------------------------------- **)
(** Context management (1) **)

(* Represents an empty context *)
let emptycontext = []

(* ctxlength returns the length of context ctx *)
let ctxlength ctx = List.length ctx

(* addbinding adds a binding bind and its symbol x to a context ctx *)
let addbinding ctx x bind = (x,bind)::ctx

(* addname adds a binding of type 'NameBind' and its symbol x to a context ctx *)
let addname ctx x = addbinding ctx x NameBind

(* isnamebound checks if the symbol x has already been used in context ctx.
   Returns true if x is bound, otherwise false is returned *)
let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

(* pickfreshname checks if symbol x has already been used in context ctx. If that
   is the case, it appends a simple quotation mark at the end of x to create a new
   symbol and try again. Once he gets a symbol that hasn't been used, he adds the 
   binding 'NameBind' and the symbol to context ctx and returns this new context 
   and the symbol as a pair *)
let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

(* index2name returns the symbol at x position on context ctx. If the symbol can
   not be found, an error message is printed *)
let index2name fi ctx x = 
    try
      let (xn,_) = List.nth ctx x in
      xn
    with Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
      error fi (msg x (List.length ctx))

(* name2index returns the position of symbol x on context ctx. If symbol x can
   not be found, and error message is printed *)
let rec name2index fi ctx x =
  let rec name2index_aux pos rest = match rest with
      [] -> error fi ("Identifier " ^ x ^ " is unbound");
     | (y,_)::rest ->
        if y=x then pos
        else name2index_aux (pos+1) rest
  in name2index_aux 0 ctx

(** ---------------------------------------------------------------------- **)
(** Shifting **)

(* defines behaviour when shifting each of the term 'types' *)
let tmmap onvar c t = 
  let rec walk c t = match t with
    TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,tp,t2) -> TmAbs(fi,x,tp,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmProj(fi,t1,l) -> TmProj(fi,walk c t1,l)
  | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->
                                               (li,walk c ti))
                                    fields)
  | TmFloat _ as t -> t
  | TmTimesfloat(fi,t1,t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
  | TmString _ as t -> t
  | TmZero(fi)      -> TmZero(fi)
  | TmSucc(fi,t1)   -> TmSucc(fi, walk c t1)
  | TmPred(fi,t1)   -> TmPred(fi, walk c t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi, walk c t1)
  | TmLet(fi,x,t1,t2) -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  in walk c t

(* defines an onvar function that is passed to tmmap and calls tmmap *)
let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d))
    c t

(* calls termShiftAbove with c = 0 *)
let termShift d t = termShiftAbove d 0 t

(* checks binding type. For a TmAbbBind performs shifting *)
let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | TmAbbBind(t,tp) -> TmAbbBind(termShift d t,tp)

(** ---------------------------------------------------------------------- **)
(** Context management (2) **)

(* getbinding returns the binding at i position on context ctx and performs a
   shifting with its position+1. If binding can not be found, an error message 
   is printed and execution ends *)
let rec getbinding fi ctx i = 
    try
      let (_,bind) = List.nth ctx i in
      bindingshift (i+1) bind 
    with Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
      error fi (msg i (List.length ctx))

let searchFromContextTerm fi ctx i = match getbinding fi ctx i with
    TmAbbBind(t,tp) -> t
  | _ -> error fi ("searchFromContextType: No term for binding " ^ (index2name fi ctx i))

let searchFromContextType fi ctx i = match getbinding fi ctx i with
    TmAbbBind(t,tp) -> match tp with
      None -> error fi ("searchFromContextType: No type recorded for binding " ^(index2name fi ctx i))
      | Some(tpT) -> tpT
  | _ -> error fi ("searchFromContextType: No term for binding " ^ (index2name fi ctx i))

(** ---------------------------------------------------------------------- **)
(** Substitution **)

(* analogous to termShiftAbove, defines an onvar function and calls tmmap *)
let termSubst j s t =
  tmmap
    (fun fi c x n -> if x=j+c then termShift c s else TmVar(fi,x,n))
    0
    t

(* defines a shift, then performs substitution, then shifts back *)
let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(** ---------------------------------------------------------------------- **)
(** Extracting file info **)

(* tmInfo returns the Support.Error.info associated with the term t *)
let tmInfo t = match t with
    TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmProj(fi,_,_) -> fi
  | TmRecord(fi,_) -> fi
  | TmFloat(fi,_) -> fi
  | TmTimesfloat(fi,_,_) -> fi
  | TmString(fi,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi
  | TmLet(fi,_,_,_) -> fi 

(** ---------------------------------------------------------------------- **)
(** Printing **)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printtm_Term outer ctx t = match t with
    TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr " then ";
       printtm_Term false ctx t2;
       print_space();
       pr " else ";
       printtm_Term false ctx t3;
       cbox()
  | TmAbs(fi,x,_,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
            obox(); pr "lambda "; pr "%s" x'; pr ". ";
            if (small t2) && not outer then break() else print_space();
            printtm_Term outer ctx' t2;
            cbox())
  | TmLet(fi, x, t1, t2) ->
       obox0();
       pr "let "; pr "%s" x; pr " = "; 
       printtm_Term false ctx t1;
       print_space(); pr "in"; print_space();
       printtm_Term false (addname ctx x) t2;
       cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      pr " ";
      printtm_ATerm false ctx t2;
      cbox()
  | TmTimesfloat(_,t1,t2) ->
       pr "timesfloat "; printtm_ATerm false ctx t1; 
       pr " "; printtm_ATerm false ctx t2
  | TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false ctx t1
  | t -> printtm_PathTerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr "%s" l
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr "%s" (index2name fi ctx x)
      else
        pr "%s" ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmRecord(fi, fields) ->
       let pf i (li,ti) =
         if (li <> ((string_of_int i))) then (pr "%s" li; pr "="); 
         printtm_Term false ctx ti 
       in let rec p i l = match l with
           [] -> ()
         | [f] -> pf i f
         | f::rest ->
             pf i f; pr","; if outer then print_space() else break(); 
             p (i+1) rest
       in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TmFloat(_,s) -> pr "%s" (string_of_float s)
  | TmString(_,s) -> pr "%s" ("\"" ^ s ^ "\"")
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr "%s" (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
     in f 1 t1
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let rec prtype ctx tp = match tp with
    TpBool -> pr "Bool"
    | TpNat -> pr "Nat"
    | TpApp(tpT1,tpT2) -> prtype ctx tpT1; pr "->"; prtype ctx tpT2 
    | _ -> pr "lol"

let rec printtype ctx term tp_opt = match tp_opt with
    None -> ()
    | Some(tp) -> prtype ctx tp

(* prbinding prints a binding depending on its type. In case it's of type
   NameBind, nothing is printed and unit is returned. For TmAbbBind type,
   the equals symbol and the binding's term are printed *)
let prbinding ctx b = match b with
    NameBind -> ()
  | TmAbbBind(t,tp) -> pr "= "; printtm ctx t; pr ":"; printtype ctx t tp
