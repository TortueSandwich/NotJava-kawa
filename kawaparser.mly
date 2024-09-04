%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PLUS MINUS TIMES DIV
%token PRINT EQ
%token VAR 
%token <string> TYPE
%token EOF

%start program
%type <Kawa.program> program

%%

program:
| globals=global_decls MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals; main} }
// | MAIN BEGIN main=list(instruction) END EOF
//     { {classes=[]; globals=[]; main} }
;

global_decls:
| { [] }
| global_decl SEMI rest=global_decls { $1 :: rest }
;

global_decl:
| VAR t=TYPE i=IDENT { (i, Kawa.typ_of_string t) }
;


instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| v=IDENT EQ e=expression SEMI{ Set(Var(v), e) }
;

expression:
| n=INT { Int(n) }
| v=IDENT { Get(Var(v)) } 
| e1=expression PLUS e2=expression {Binop(Add, e1, e2)}
| e1=expression MINUS e2=expression {Binop(Sub, e1, e2)}
| e1=expression TIMES e2=expression {Binop(Mul, e1, e2)}
| e1=expression DIV e2=expression {Binop(Div, e1, e2)}
| LPAR e=expression RPAR { e }
;
