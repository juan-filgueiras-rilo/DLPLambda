(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(******************************************************************************
 *                                                                            
 * Module Core	                                                              
 *                                                                            
 * Core typechecking and evaluation functions                                 
 *                                                                            
 *****************************************************************************)

open Format
open Syntax
open Support.Error
open Support.Pervasive

(** ------------------------   EVALUATION  ------------------------ **)

exception NoRuleApplies

let debug = ref false

(* isnumericval returns true if term t is a number or false if not *)
let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

(* isval returns true if the term t is a value or false if not *)
let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmFloat _  -> true
  | TmString _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | _ -> false

(* eval1 evaluates one step forward the term t *)
let rec eval1 ctx t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t) -> t 
        | _ -> raise NoRuleApplies)
  | TmApp(fi,TmAbs(_,x,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi,t1,t2') 
  | TmTimesfloat(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi,t1',t2) 
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | _ -> 
      raise NoRuleApplies

(* eval evaluates the term t until it cannot be evaluated anymore. If debug mode is activated,
   it also prints how the terms are being evaluated *) 
let rec eval ctx t = 
  if !debug then begin
    if not (isval ctx t) then begin pr "("; printtm ctx t; pr ") -> "
    end else ();
      try let t' = eval1 ctx t;
      in eval ctx t'
    with NoRuleApplies -> t
  end else
    try let t' = eval1 ctx t;
      in eval ctx t'
    with NoRuleApplies -> t

(* evalbinding evaluates the term contained by the binding until it cannot be evaluated anymore *)
let evalbinding ctx b = match b with
    TmAbbBind(t) ->
      let t' = eval ctx t in 
      TmAbbBind(t')
  | bind -> bind

(** ------------------------   TYPING  ------------------------ **)
let rec gettype ctx t = match t with
    TmTrue(_) ->
      TpBool
  | TmFalse(_) ->
      TpBool
  | TmIf(fi,t1,t2,t3) ->
      if ((gettype ctx t1) = TpBool)
      then let tpBody = (gettype ctx t2) in
        if tpBody = (gettype ctx t3)
        then tpBody
        else error fi "tipos diferentes en las ramas del if"
      else error fi "la condicion no es de tipo Bool"
  | t when isnumericval ctx t  -> 
      TpNat
  | TmVar(fi,i,_) -> gettype ctx (searchFromContextTerm fi ctx i)
  | TmApp(fi,t1,t2) -> 
      let tpT1 = (gettype ctx t1) in
      let tpT2 = (gettype ctx t2) in
      (match tpT1 with
          TpApp(tpT11,tpT12) -> 
            if tpT2 = tpT11 then tpT12
            else error fi "input parameter doesn't match"
        | _ -> error fi "expected type for application")
  | TmSucc(fi,t1) ->
      let tp = gettype ctx t1 in
        match tp with
            TpNat -> TpNat
          | _ -> error fi "expected value of type Nat" 