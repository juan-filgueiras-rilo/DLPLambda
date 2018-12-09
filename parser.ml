type token =
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | LET of (Support.Error.info)
  | IN of (Support.Error.info)
  | TBOOL of (Support.Error.info)
  | TNAT of (Support.Error.info)
  | UCID of (string Support.Error.withinfo)
  | LCID of (string Support.Error.withinfo)
  | INTV of (int Support.Error.withinfo)
  | FLOATV of (float Support.Error.withinfo)
  | STRINGV of (string Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 14 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
open Core
# 67 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* IF *);
  258 (* THEN *);
  259 (* ELSE *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* LAMBDA *);
  263 (* TIMESFLOAT *);
  264 (* SUCC *);
  265 (* PRED *);
  266 (* ISZERO *);
  267 (* LET *);
  268 (* IN *);
  269 (* TBOOL *);
  270 (* TNAT *);
  271 (* UCID *);
  272 (* LCID *);
  273 (* INTV *);
  274 (* FLOATV *);
  275 (* STRINGV *);
  276 (* APOSTROPHE *);
  277 (* DQUOTE *);
  278 (* ARROW *);
  279 (* BANG *);
  280 (* BARGT *);
  281 (* BARRCURLY *);
  282 (* BARRSQUARE *);
  283 (* COLON *);
  284 (* COLONCOLON *);
  285 (* COLONEQ *);
  286 (* COLONHASH *);
  287 (* COMMA *);
  288 (* DARROW *);
  289 (* DDARROW *);
  290 (* DOT *);
    0 (* EOF *);
  291 (* EQ *);
  292 (* EQEQ *);
  293 (* EXISTS *);
  294 (* GT *);
  295 (* HASH *);
  296 (* LCURLY *);
  297 (* LCURLYBAR *);
  298 (* LEFTARROW *);
  299 (* LPAREN *);
  300 (* LSQUARE *);
  301 (* LSQUAREBAR *);
  302 (* LT *);
  303 (* RCURLY *);
  304 (* RPAREN *);
  305 (* RSQUARE *);
  306 (* SEMI *);
  307 (* SLASH *);
  308 (* STAR *);
  309 (* TRIANGLE *);
  310 (* USCORE *);
  311 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\005\000\005\000\005\000\004\000\004\000\
\002\000\002\000\007\000\007\000\006\000\006\000\006\000\006\000\
\006\000\006\000\008\000\008\000\008\000\008\000\008\000\008\000\
\009\000\009\000\009\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\012\000\012\000\013\000\
\013\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\001\000\003\000\001\000\
\001\000\002\000\001\000\002\000\001\000\006\000\006\000\006\000\
\006\000\006\000\001\000\002\000\003\000\002\000\002\000\002\000\
\003\000\003\000\001\000\003\000\001\000\001\000\001\000\003\000\
\001\000\001\000\001\000\000\000\001\000\001\000\003\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\029\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\033\000\034\000\001\000\
\000\000\000\000\042\000\000\000\009\000\000\000\000\000\027\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\010\000\000\000\041\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\032\000\000\000\
\028\000\002\000\025\000\026\000\000\000\005\000\006\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\040\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\004\000\015\000\007\000\016\000\017\000\018\000"

let yydgoto = "\002\000\
\019\000\020\000\065\000\066\000\067\000\021\000\037\000\022\000\
\023\000\024\000\040\000\041\000\042\000"

let yysindex = "\007\000\
\001\000\000\000\053\000\000\000\000\000\242\254\098\255\098\255\
\098\255\098\255\243\254\227\254\000\000\000\000\000\000\000\000\
\072\000\053\000\000\000\224\254\000\000\098\255\249\254\000\000\
\000\000\018\255\002\255\003\255\082\000\249\254\249\254\249\254\
\252\254\254\254\053\000\000\000\000\000\001\255\000\000\244\254\
\000\000\006\255\253\254\001\000\249\254\000\255\053\000\247\254\
\247\254\249\254\053\000\053\000\000\000\053\000\000\000\072\000\
\000\000\000\000\000\000\000\000\036\255\000\000\000\000\247\254\
\012\255\000\000\034\255\024\255\047\255\048\255\000\000\000\000\
\053\000\013\255\053\000\247\254\053\000\053\000\053\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\187\255\000\000\000\000\000\000\000\000\
\015\255\000\000\000\000\000\000\000\000\040\255\007\255\000\000\
\000\000\000\000\000\000\000\000\000\000\061\255\079\255\128\255\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\020\255\000\000\000\000\146\255\000\000\000\000\000\000\
\000\000\195\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\229\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\024\000\000\000\220\255\249\255\000\000\253\255\000\000\000\000\
\145\000\000\000\000\000\029\000\000\000"

let yytablesize = 381
let yytable = "\026\000\
\016\000\027\000\033\000\062\000\063\000\035\000\008\000\001\000\
\019\000\019\000\019\000\019\000\068\000\039\000\043\000\059\000\
\060\000\044\000\019\000\047\000\008\000\036\000\019\000\019\000\
\019\000\019\000\046\000\074\000\048\000\049\000\051\000\053\000\
\052\000\064\000\055\000\054\000\056\000\019\000\073\000\028\000\
\034\000\013\000\013\000\061\000\057\000\075\000\019\000\069\000\
\070\000\019\000\071\000\013\000\039\000\019\000\019\000\076\000\
\019\000\077\000\078\000\079\000\081\000\036\000\022\000\022\000\
\022\000\022\000\038\000\058\000\083\000\080\000\013\000\082\000\
\022\000\084\000\085\000\086\000\022\000\022\000\022\000\022\000\
\023\000\023\000\023\000\023\000\072\000\000\000\013\000\013\000\
\000\000\013\000\023\000\022\000\000\000\000\000\023\000\023\000\
\023\000\023\000\000\000\000\000\022\000\004\000\005\000\022\000\
\000\000\000\000\000\000\022\000\022\000\023\000\022\000\000\000\
\000\000\025\000\013\000\014\000\015\000\000\000\023\000\000\000\
\000\000\023\000\000\000\000\000\000\000\023\000\023\000\000\000\
\023\000\024\000\024\000\024\000\024\000\000\000\000\000\000\000\
\000\000\017\000\000\000\024\000\018\000\000\000\000\000\024\000\
\024\000\024\000\024\000\020\000\020\000\020\000\020\000\029\000\
\030\000\031\000\032\000\000\000\000\000\020\000\024\000\000\000\
\000\000\020\000\020\000\020\000\020\000\000\000\045\000\024\000\
\000\000\000\000\024\000\000\000\000\000\050\000\024\000\024\000\
\020\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\000\000\000\000\020\000\000\000\031\000\031\000\
\020\000\020\000\000\000\020\000\021\000\021\000\021\000\021\000\
\000\000\000\000\031\000\031\000\031\000\031\000\021\000\000\000\
\000\000\000\000\021\000\021\000\021\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\021\000\031\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\021\000\000\000\031\000\021\000\000\000\000\000\
\000\000\021\000\021\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\004\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\031\000\031\000\000\000\000\000\
\012\000\013\000\014\000\015\000\000\000\000\000\000\000\000\000\
\031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\017\000\000\000\031\000\018\000\000\000\000\000\000\000\000\000\
\031\000\000\000\000\000\031\000\000\000\003\000\000\000\031\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\000\000\000\000\000\000\000\000\025\000\013\000\014\000\015\000\
\003\000\000\000\000\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\000\000\000\000\004\000\005\000\038\000\
\013\000\014\000\015\000\000\000\017\000\000\000\000\000\018\000\
\000\000\025\000\013\000\014\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\018\000\046\000\000\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\018\000"

let yycheck = "\003\000\
\000\000\016\001\016\001\013\001\014\001\035\001\034\001\001\000\
\002\001\003\001\004\001\005\001\049\000\017\000\018\000\016\001\
\017\001\050\001\012\001\002\001\048\001\051\001\016\001\017\001\
\018\001\019\001\034\001\064\000\027\001\027\001\035\001\035\000\
\035\001\043\001\047\001\035\001\031\001\031\001\003\001\054\001\
\054\001\002\001\003\001\047\000\048\001\034\001\040\001\051\000\
\052\000\043\001\054\000\012\001\056\000\047\001\048\001\022\001\
\050\001\034\001\012\001\012\001\048\001\047\001\002\001\003\001\
\004\001\005\001\047\001\044\000\076\000\073\000\031\001\075\000\
\012\001\077\000\078\000\079\000\016\001\017\001\018\001\019\001\
\002\001\003\001\004\001\005\001\056\000\255\255\047\001\048\001\
\255\255\050\001\012\001\031\001\255\255\255\255\016\001\017\001\
\018\001\019\001\255\255\255\255\040\001\004\001\005\001\043\001\
\255\255\255\255\255\255\047\001\048\001\031\001\050\001\255\255\
\255\255\016\001\017\001\018\001\019\001\255\255\040\001\255\255\
\255\255\043\001\255\255\255\255\255\255\047\001\048\001\255\255\
\050\001\002\001\003\001\004\001\005\001\255\255\255\255\255\255\
\255\255\040\001\255\255\012\001\043\001\255\255\255\255\016\001\
\017\001\018\001\019\001\002\001\003\001\004\001\005\001\007\000\
\008\000\009\000\010\000\255\255\255\255\012\001\031\001\255\255\
\255\255\016\001\017\001\018\001\019\001\255\255\022\000\040\001\
\255\255\255\255\043\001\255\255\255\255\029\000\047\001\048\001\
\031\001\050\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\040\001\255\255\255\255\043\001\255\255\004\001\005\001\
\047\001\048\001\255\255\050\001\002\001\003\001\004\001\005\001\
\255\255\255\255\016\001\017\001\018\001\019\001\012\001\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\255\255\255\255\255\255\034\001\255\255\255\255\255\255\
\255\255\031\001\040\001\255\255\255\255\043\001\255\255\255\255\
\255\255\255\255\040\001\255\255\050\001\043\001\255\255\255\255\
\255\255\047\001\048\001\255\255\050\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\004\001\005\001\255\255\255\255\
\016\001\017\001\018\001\019\001\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\031\001\
\040\001\255\255\034\001\043\001\255\255\255\255\255\255\255\255\
\040\001\255\255\255\255\043\001\255\255\001\001\255\255\047\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\001\001\255\255\255\255\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\004\001\005\001\016\001\
\017\001\018\001\019\001\255\255\040\001\255\255\255\255\043\001\
\255\255\016\001\017\001\018\001\019\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\040\001\
\255\255\255\255\043\001\034\001\255\255\255\255\255\255\255\255\
\255\255\040\001\255\255\255\255\043\001"

let yynames_const = "\
  "

let yynames_block = "\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  LAMBDA\000\
  TIMESFLOAT\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  IN\000\
  TBOOL\000\
  TNAT\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 125 "parser.mly"
      ( fun ctx -> [],ctx )
# 363 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 130 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 375 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppType) in
    Obj.repr(
# 137 "parser.mly"
    ( _1 )
# 382 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 141 "parser.mly"
    ( _2 )
# 391 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 143 "parser.mly"
    (fun ctx -> TpBool)
# 398 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
    (fun ctx -> TpNat)
# 405 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppType) in
    Obj.repr(
# 149 "parser.mly"
    ( fun ctx  -> TpApp(_1 ctx, _3 ctx))
# 414 "parser.ml"
               : 'AppType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 151 "parser.mly"
    ( _1 )
# 421 "parser.ml"
               : 'AppType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 157 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 428 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 162 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 436 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 169 "parser.mly"
      ( fun ctx -> NameBind )
# 443 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 173 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 451 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 178 "parser.mly"
      ( _1 )
# 458 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 182 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 470 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 186 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 484 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 191 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 498 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 197 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 510 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 200 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 522 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 205 "parser.mly"
      ( _1 )
# 529 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 208 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 540 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 214 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 549 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 217 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 557 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 220 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 565 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 223 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 573 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 229 "parser.mly"
      ( fun ctx ->       
          TmProj(_2, _1 ctx, _3.v) )
# 583 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 234 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 593 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 238 "parser.mly"
      ( _1 )
# 600 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 244 "parser.mly"
      ( _2 )
# 609 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 247 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 616 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 250 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 623 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 254 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 631 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 258 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 641 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 262 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 648 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 265 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 655 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 268 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 666 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 276 "parser.mly"
      ( fun ctx i -> [] )
# 672 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 279 "parser.mly"
      ( _1 )
# 679 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 285 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 686 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 288 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 695 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 293 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 704 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 296 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 711 "parser.ml"
               : 'Field))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.context -> (Syntax.command list * Syntax.context) )
