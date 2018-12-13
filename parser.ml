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
  | LETREC of (Support.Error.info)
  | TBOOL of (Support.Error.info)
  | TNAT of (Support.Error.info)
  | TSTRING of (Support.Error.info)
  | TFLOAT of (Support.Error.info)
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
# 70 "parser.ml"
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
  269 (* LETREC *);
  270 (* TBOOL *);
  271 (* TNAT *);
  272 (* TSTRING *);
  273 (* TFLOAT *);
  274 (* UCID *);
  275 (* LCID *);
  276 (* INTV *);
  277 (* FLOATV *);
  278 (* STRINGV *);
  279 (* APOSTROPHE *);
  280 (* DQUOTE *);
  281 (* ARROW *);
  282 (* BANG *);
  283 (* BARGT *);
  284 (* BARRCURLY *);
  285 (* BARRSQUARE *);
  286 (* COLON *);
  287 (* COLONCOLON *);
  288 (* COLONEQ *);
  289 (* COLONHASH *);
  290 (* COMMA *);
  291 (* DARROW *);
  292 (* DDARROW *);
  293 (* DOT *);
    0 (* EOF *);
  294 (* EQ *);
  295 (* EQEQ *);
  296 (* EXISTS *);
  297 (* GT *);
  298 (* HASH *);
  299 (* LCURLY *);
  300 (* LCURLYBAR *);
  301 (* LEFTARROW *);
  302 (* LPAREN *);
  303 (* LSQUARE *);
  304 (* LSQUAREBAR *);
  305 (* LT *);
  306 (* RCURLY *);
  307 (* RPAREN *);
  308 (* RSQUARE *);
  309 (* SEMI *);
  310 (* SLASH *);
  311 (* STAR *);
  312 (* TRIANGLE *);
  313 (* USCORE *);
  314 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\005\000\005\000\005\000\005\000\005\000\
\004\000\004\000\002\000\002\000\007\000\007\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\009\000\009\000\009\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\001\000\001\000\001\000\
\003\000\001\000\001\000\002\000\001\000\002\000\001\000\006\000\
\006\000\006\000\006\000\006\000\008\000\001\000\002\000\003\000\
\002\000\002\000\002\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\003\000\001\000\001\000\001\000\000\000\001\000\
\001\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\032\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\036\000\037\000\
\001\000\000\000\000\000\045\000\000\000\011\000\000\000\000\000\
\030\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\012\000\000\000\
\044\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\000\000\035\000\000\000\031\000\002\000\028\000\029\000\000\000\
\005\000\006\000\007\000\008\000\000\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\043\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\004\000\
\017\000\009\000\018\000\019\000\020\000\000\000\000\000\021\000"

let yydgoto = "\002\000\
\020\000\021\000\070\000\071\000\072\000\022\000\039\000\023\000\
\024\000\025\000\042\000\043\000\044\000"

let yysindex = "\002\000\
\001\000\000\000\078\000\000\000\000\000\239\254\020\000\020\000\
\020\000\020\000\248\254\250\254\235\254\000\000\000\000\000\000\
\000\000\106\000\078\000\000\000\221\254\000\000\020\000\244\254\
\000\000\000\000\018\255\005\255\008\255\118\000\244\254\244\254\
\244\254\255\254\001\255\014\255\078\000\000\000\000\000\004\255\
\000\000\249\254\000\000\027\255\252\254\001\000\244\254\012\255\
\078\000\013\255\013\255\244\254\078\000\078\000\013\255\000\000\
\078\000\000\000\106\000\000\000\000\000\000\000\000\000\059\255\
\000\000\000\000\000\000\000\000\013\255\026\255\000\000\039\255\
\034\255\077\255\078\255\053\255\000\000\000\000\078\000\042\255\
\078\000\013\255\078\000\078\000\078\000\078\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\082\255\078\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\046\255\000\000\000\000\000\000\000\000\007\255\002\255\
\000\000\000\000\000\000\000\000\000\000\000\000\065\255\100\255\
\135\255\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\000\000\000\000\047\255\000\000\000\000\170\255\000\000\
\000\000\000\000\000\000\206\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\052\000\000\000\213\255\018\000\000\000\253\255\000\000\000\000\
\065\000\000\000\000\000\042\000\000\000"

let yytablesize = 420
let yytable = "\027\000\
\017\000\028\000\001\000\022\000\022\000\022\000\022\000\073\000\
\015\000\015\000\034\000\076\000\036\000\022\000\041\000\045\000\
\037\000\046\000\015\000\049\000\022\000\022\000\022\000\022\000\
\048\000\080\000\065\000\066\000\067\000\068\000\062\000\063\000\
\038\000\056\000\050\000\022\000\053\000\051\000\054\000\029\000\
\015\000\057\000\058\000\055\000\022\000\064\000\060\000\022\000\
\035\000\074\000\075\000\022\000\022\000\077\000\022\000\041\000\
\015\000\015\000\069\000\015\000\059\000\079\000\081\000\082\000\
\010\000\010\000\025\000\025\000\025\000\025\000\083\000\030\000\
\031\000\032\000\033\000\087\000\025\000\089\000\010\000\091\000\
\092\000\093\000\094\000\025\000\025\000\025\000\025\000\047\000\
\084\000\085\000\086\000\096\000\088\000\095\000\052\000\039\000\
\041\000\061\000\025\000\090\000\078\000\026\000\026\000\026\000\
\026\000\000\000\000\000\025\000\000\000\000\000\025\000\026\000\
\000\000\000\000\025\000\025\000\000\000\025\000\026\000\026\000\
\026\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\000\000\
\027\000\027\000\027\000\027\000\000\000\000\000\026\000\000\000\
\000\000\026\000\027\000\000\000\000\000\026\000\026\000\000\000\
\026\000\027\000\027\000\027\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\000\000\023\000\023\000\023\000\023\000\000\000\
\000\000\027\000\000\000\000\000\027\000\023\000\000\000\000\000\
\027\000\027\000\000\000\027\000\023\000\023\000\023\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000\000\000\023\000\000\000\000\000\023\000\
\000\000\024\000\000\000\023\000\023\000\000\000\023\000\000\000\
\024\000\024\000\024\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\024\000\000\000\000\000\000\000\024\000\
\024\000\003\000\024\000\000\000\004\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\000\000\012\000\034\000\034\000\
\000\000\000\000\000\000\013\000\014\000\015\000\016\000\004\000\
\005\000\000\000\000\000\000\000\000\000\034\000\034\000\034\000\
\034\000\000\000\034\000\034\000\000\000\000\000\026\000\014\000\
\015\000\016\000\000\000\018\000\000\000\000\000\019\000\034\000\
\000\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\018\000\034\000\
\034\000\019\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\034\000\000\000\000\000\034\000\000\000\003\000\000\000\
\034\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\026\000\014\000\015\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\000\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\000\000\012\000\000\000\
\018\000\004\000\005\000\019\000\040\000\014\000\015\000\016\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\014\000\015\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\019\000\
\000\000\000\000\048\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\000\000\019\000"

let yycheck = "\003\000\
\000\000\019\001\001\000\002\001\003\001\004\001\005\001\051\000\
\002\001\003\001\019\001\055\000\019\001\012\001\018\000\019\000\
\038\001\053\001\012\001\002\001\019\001\020\001\021\001\022\001\
\037\001\069\000\014\001\015\001\016\001\017\001\019\001\020\001\
\054\001\037\000\030\001\034\001\038\001\030\001\038\001\057\001\
\034\001\038\001\050\001\030\001\043\001\049\000\051\001\046\001\
\057\001\053\000\054\000\050\001\051\001\057\000\053\001\059\000\
\050\001\051\001\046\001\053\001\034\001\003\001\037\001\025\001\
\037\001\038\001\002\001\003\001\004\001\005\001\037\001\007\000\
\008\000\009\000\010\000\079\000\012\001\081\000\051\001\083\000\
\084\000\085\000\086\000\019\001\020\001\021\001\022\001\023\000\
\012\001\012\001\038\001\095\000\051\001\012\001\030\000\050\001\
\050\001\046\000\034\001\082\000\059\000\002\001\003\001\004\001\
\005\001\255\255\255\255\043\001\255\255\255\255\046\001\012\001\
\255\255\255\255\050\001\051\001\255\255\053\001\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\034\001\255\255\255\255\
\002\001\003\001\004\001\005\001\255\255\255\255\043\001\255\255\
\255\255\046\001\012\001\255\255\255\255\050\001\051\001\255\255\
\053\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\034\001\255\255\255\255\002\001\003\001\004\001\005\001\255\255\
\255\255\043\001\255\255\255\255\046\001\012\001\255\255\255\255\
\050\001\051\001\255\255\053\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\034\001\255\255\255\255\255\255\002\001\
\003\001\004\001\005\001\255\255\043\001\255\255\255\255\046\001\
\255\255\012\001\255\255\050\001\051\001\255\255\053\001\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\043\001\255\255\255\255\046\001\255\255\255\255\255\255\050\001\
\051\001\001\001\053\001\255\255\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\004\001\005\001\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\004\001\
\005\001\255\255\255\255\255\255\255\255\019\001\020\001\021\001\
\022\001\255\255\004\001\005\001\255\255\255\255\019\001\020\001\
\021\001\022\001\255\255\043\001\255\255\255\255\046\001\037\001\
\255\255\019\001\020\001\021\001\022\001\043\001\255\255\255\255\
\046\001\255\255\255\255\255\255\255\255\255\255\043\001\053\001\
\034\001\046\001\255\255\037\001\255\255\255\255\255\255\255\255\
\255\255\043\001\255\255\255\255\046\001\255\255\001\001\255\255\
\050\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\013\001\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\255\255\255\255\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\255\255\
\043\001\004\001\005\001\046\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\043\001\255\255\255\255\046\001\
\255\255\255\255\037\001\255\255\255\255\255\255\255\255\255\255\
\043\001\255\255\255\255\046\001"

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
  LETREC\000\
  TBOOL\000\
  TNAT\000\
  TSTRING\000\
  TFLOAT\000\
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
# 127 "parser.mly"
      ( fun ctx -> [],ctx )
# 385 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 132 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 397 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppType) in
    Obj.repr(
# 139 "parser.mly"
    ( _1 )
# 404 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 143 "parser.mly"
    ( _2 )
# 413 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
    (fun ctx -> TpBool)
# 420 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 147 "parser.mly"
    (fun ctx -> TpNat)
# 427 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 149 "parser.mly"
    (fun ctx -> TpString)
# 434 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 151 "parser.mly"
    (fun ctx -> TpFloat)
# 441 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppType) in
    Obj.repr(
# 155 "parser.mly"
    ( fun ctx  -> TpApp(_1 ctx, _3 ctx))
# 450 "parser.ml"
               : 'AppType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 157 "parser.mly"
    ( _1 )
# 457 "parser.ml"
               : 'AppType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 163 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 464 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 168 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 472 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 175 "parser.mly"
      ( fun ctx -> NameBind )
# 479 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 179 "parser.mly"
      ( fun ctx -> TmAbbBind(Some(_2 ctx), None) )
# 487 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 184 "parser.mly"
      ( _1 )
# 494 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 188 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 506 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 192 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 520 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 197 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 534 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 203 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 546 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 206 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 558 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 208 "parser.mly"
      ( fun ctx -> 
          let ctx1 = addname ctx _2.v in 
          TmLet(_1, _2.v, TmFix(_1, TmAbs(_1, _2.v, _4 ctx, _6 ctx1)),
                _8 ctx1) )
# 575 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 215 "parser.mly"
      ( _1 )
# 582 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 218 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 593 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 224 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 602 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 227 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 610 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 230 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 618 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 233 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 626 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 239 "parser.mly"
      ( fun ctx ->       
          TmProj(_2, _1 ctx, _3.v) )
# 636 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 244 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 646 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 248 "parser.mly"
      ( _1 )
# 653 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 254 "parser.mly"
      ( _2 )
# 662 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 257 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 669 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 260 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 676 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 264 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 684 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 268 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 694 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 272 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 701 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 275 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 708 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 278 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 719 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 286 "parser.mly"
      ( fun ctx i -> [] )
# 725 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 289 "parser.mly"
      ( _1 )
# 732 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 295 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 739 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 298 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 748 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 303 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 757 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 306 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 764 "parser.ml"
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
