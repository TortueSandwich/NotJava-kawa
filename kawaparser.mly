%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PLUS MINUS TIMES DIV MOD
%token PRINT EQ NOTEQ
%token VAR 
%token IF ELSE WHILE
%token <string> TYPE
%token EOF

%nonassoc EQ NOTEQ
%left TIMES DIV MOD
%right PLUS MINUS

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
| v=IDENT EQ e=expression SEMI { Set(Var(v), e) }
// | WHILE LPAR e=expression RPAR rest=instruction { While(e, rest :: [])}
| WHILE e=expression BEGIN rest=instruction_seq END { While(e, rest) }
// | IF LPAR e=expression RPAR rest=instruction {If(e, rest :: [], [])}
| IF e=expression BEGIN rest=instruction_seq END { If(e, rest, []) }
| IF e=expression BEGIN rest1=instruction_seq END ELSE BEGIN rest2=instruction_seq END { If(e, rest1, rest2) }
;

instruction_seq:
| { [] }
| instruction rest=instruction_seq { $1 :: rest }
;

expression:
| n=INT { Int(n) }
| v=IDENT { Get(Var(v)) } 
| e1=expression NOTEQ e2=expression { Binop(Neq, e1, e2) }
| e1=expression PLUS e2=expression { Binop(Add, e1, e2) }
| e1=expression MINUS e2=expression { Binop(Sub, e1, e2) }
| e1=expression TIMES e2=expression { Binop(Mul, e1, e2) }
| e1=expression DIV e2=expression { Binop(Div, e1, e2) }
| e1=expression MOD e2=expression { Binop(Rem, e1, e2) }
| LPAR e=expression RPAR { e }
;
