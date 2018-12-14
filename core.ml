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
  | TmAbs(_,_,_,_) -> true
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
          TmAbbBind(Some(t),_) -> t 
        | _ -> raise NoRuleApplies)
  | TmApp(fi,TmAbs(_,x,_,t12),v2) when isval ctx v2 ->
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
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
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
    TmAbbBind(Some(t),tp) ->
      let t' = eval ctx t in 
      TmAbbBind(Some(t'),tp)
  | bind -> bind

(** ------------------------   TYPING  ------------------------ **)
let rec gettype ctx t = match t with
    TmTrue(fi) ->
      TpBool
  | TmFalse(fi) ->
      TpBool
  | TmZero(fi) -> 
      TpNat
(*   | t when isnumericval ctx t  -> 
      TpNat *)
  | TmIf(fi,t1,t2,t3) ->
      if ((gettype ctx t1) = TpBool)
      then let tpBody = (gettype ctx t2) in
        if tpBody = (gettype ctx t3)
        then tpBody
        else error fi "tipos diferentes en las ramas del if"
      else error fi "la condicion no es de tipo Bool"
  | TmSucc(fi,t1) ->
      let tp = gettype ctx t1 in
        (match tp with
            TpNat -> TpNat
          | _ -> error fi "expected value of type Nat for succ")
  | TmPred(fi,t1) ->
        let tp = gettype ctx t1 in
        (match tp with
            TpNat -> TpNat
          | _ -> error fi "expected value of type Nat for pred")
  | TmIsZero(fi,t1) ->
      let tp = gettype ctx t1 in
        (match tp with
            TpNat -> TpBool
          | _ -> error fi "expected value of type Nat for iszero")
  | TmString(fi,_) -> TpString
  | TmFloat(fi,_) -> TpFloat
  | TmTimesfloat(fi,t1,t2) ->
      let tpT1 = (gettype ctx t1) in
      let tpT2 = (gettype ctx t2) in
      if tpT1 = TpFloat
      then 
        if tpT2 = TpFloat 
        then TpFloat
        else error fi "second argument of timesfloat is not a Float"
      else error fi "first argument of timesfloat is not a Float"
  | TmVar(fi,i,_) -> searchFromContextType fi ctx i
  | TmAbs(fi,x,tpT1,t1) ->
      let ctx' = addbinding ctx x (TmAbbBind(None,(Some(tpT1)))) in
      let tpT2 = gettype ctx' t1 in
        TpApp(tpT1, typeShift (-1) tpT2) 
  | TmApp(fi,t1,t2) -> 
      let tpT1 = (gettype ctx t1) in
      let tpT2 = (gettype ctx t2) in
      (match tpT1 with
          TpApp(tpT11,tpT12) -> 
            if tpT2 = tpT11 then tpT12
            else error fi "input parameter doesn't match"
        | _ -> error fi "arrow type expected")
  | TmLet(fi,x,t1,t2) ->
      let tpT1 = gettype ctx t1 in
      let ctx' = addbinding ctx x (TmAbbBind(Some(t1),(Some(tpT1)))) in         
        gettype ctx' t2
  | TmRecord(fi, fields) ->
      let fieldtypes = 
      (*Mapeo cada tipo de termino en un tipo registro*)
        List.map (fun (li,ti) -> (li, gettype ctx ti)) fields in
      TpRecord(fieldtypes)
  | TmProj(fi, t1, l) ->
      (match gettype ctx t1 with
          TpRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmFix(fi, t1) ->
      let tpT1 = gettype ctx t1 in
      (match tpT1 with
          TpApp(tpT11,tpT12) ->
            if (tpT12) = (tpT11) then tpT12
            else error fi "result of body not compatible with domain"
        | _ -> error fi "arrow type expected")
