/*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*/

/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
open Core
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> TIMESFLOAT
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> LET
%token <Support.Error.info> IN

%token <Support.Error.info> TBOOL
%token <Support.Error.info> TNAT


/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      /* Yacc being a bottom-up LR(1) parser, starts from the bottom,
        so EOF will be the first to be found and an empy list created. */
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      /* Whenever a new command followed by a semicolon (;) is completed,
        the command is appended to the beginning of the list of commands 
        returned by toplevel. */
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

Type :
  AppType
    { $1 }

AType :
  LPAREN Type RPAREN  
    { $2 }
  | TBOOL
    {fun ctx -> TpBool}
  | TNAT
    {fun ctx -> TpNat}

AppType :
  AType ARROW AppType
    { fun ctx  -> TpApp($1 ctx, $3 ctx)} 
  | AType
    { $1 }
/* A top-level command */
Command :
  | Term 
    /* If a term is parsed, it is returned as an Evaluation of t
      in the current context. */
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | LCID Binder
    /* If the command is a lowercase word and a binder, the command is 
      returned as a Bind on current context and its name added to the
      context */
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    SLASH
    /* A word followed by a slash (/) serves as a NameBind,
      its name is already added to the context in the Command rule. */
      { fun ctx -> NameBind }
  | EQ Term
    /* If the binder is in the form "= Term", it is returned as 
      a TmAbbBind of Term on current context. */
      { fun ctx -> TmAbbBind($2 ctx, None) }

Term :
    AppTerm
    /* An application is just returned after being parsed. */
      { $1 }
  | IF Term THEN Term ELSE Term
    /* An "if-then-else" Term is returned as a TmIf command 
      to evaluate the Terms on current context. */
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
  | LAMBDA LCID COLON Type DOT Term 
    /* An abstraction matching "lambda word . Term" adds word to the context
      and returns a TmAbs with the word and term on the new context. */
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
    /* An abstraction using underscore (_) is treated like a variable whose name can be disregarded. */
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | LET LCID EQ Term IN Term
    /* A "let word = Term in Term" clause is returned as a TmLet of the terms applied on current context 
      and the name of the last term added to the context */
      { fun ctx -> TmLet($1, $2.v, $4 ctx, $6 (addname ctx $2.v)) }
  | LET USCORE EQ Term IN Term
    /* If a "let-in" is fed an underscore it simply disregards the variable name */
      { fun ctx -> TmLet($1, "_", $4 ctx, $6 (addname ctx "_")) }

AppTerm :
    PathTerm
      /* A parsed PathTerm is returned as-is */
      { $1 }
  | AppTerm PathTerm
      /* An application of an application is parsed deeper and returned as a TmApp */
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | TIMESFLOAT PathTerm PathTerm
      /* A float multiplication of two parsed PathTerms is returned as a TmTimesfloat */
      { fun ctx -> TmTimesfloat($1, $2 ctx, $3 ctx) }
  | SUCC PathTerm
      /* Successor of a parsed PathTerm becomes TmSucc of said PathTerm applied to current context */
      { fun ctx -> TmSucc($1, $2 ctx) }
  | PRED PathTerm
      /* Predecessor of a parsed PathTerm becomes TmPred of said PathTerm applied to current context */
      { fun ctx -> TmPred($1, $2 ctx) }
  | ISZERO PathTerm
      /* Checking if a parsed PathTerm is zero returns a TmIsZero of it applied to current context */
      { fun ctx -> TmIsZero($1, $2 ctx) }

PathTerm :
    PathTerm DOT LCID
      /* A projection "Term.word" is returned as a TmProj located where the . is and using 
        Term on current context and said word */
      { fun ctx ->       
          TmProj($2, $1 ctx, $3.v) }
  | PathTerm DOT INTV
        /* A projection "Term.integer" is returned as a TmProj located where the . is and using 
        Term on current context and said integer value */ 
      { fun ctx ->
          TmProj($2, $1 ctx, string_of_int $3.v) }
  | ATerm
      /* A parsed atomic term is returned as-is */
      { $1 }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      /* Any Term between parentheses () is returned as-is */
      { $2 } 
  | TRUE
      /* True is returned as a TmTrue with its location information */
      { fun ctx -> TmTrue($1) }
  | FALSE
      /* False is returned as a TmFalse with its location information */
      { fun ctx -> TmFalse($1) }      
  | LCID 
      /* An unreserved word is returned as a TmVar variable using its information,
        location in the context and context length */
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | LCURLY Fields RCURLY
      /* A list of fields surrounded by curly brackets {} is returned as a TmRecord*/
      { fun ctx ->
          TmRecord($1, $2 ctx 1) }
  | FLOATV
      /* Any float is returned as TmFloat with its location and value */
      { fun ctx -> TmFloat($1.i, $1.v) }
  | STRINGV
      /* Any string literal is returned as a TmString with its location and content */
      { fun ctx -> TmString($1.i, $1.v) }
  | INTV
      /* An integer can either be zero or a succession of successors of zero */
      { fun ctx ->
          let rec f n = match n with
              0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n-1))
          in f $1.v }

Fields :
    /* Empty list of fields returns an empty list */
      { fun ctx i -> [] }
  | NEFields
    /* A non-empty list of fields would've been parsed below and is returned as-is */
      { $1 }

NEFields :
    /* The "last" field is returned applied in context and an index as
      a list member. */
    Field
      { fun ctx i -> [$1 ctx i] }
    /* Each field is parsed and appended at the head of the list of other fields */
  | Field COMMA NEFields
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field :
    /* A named field is returned with its name */
    LCID EQ Term
      { fun ctx i -> ($1.v, $3 ctx) }
    /* A simple term is returned using its index */
  | Term
      { fun ctx i -> (string_of_int i, $1 ctx) }


/*   */
