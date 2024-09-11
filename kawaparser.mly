%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <bool> BOOL
%token <string> TYPE IDENT
%token MAIN PRINT 
%token LPAR RPAR BEGIN END SEMI
%token PLUS MINUS TIMES DIV MOD EQ NEQ
%token VAR AFFECT 
%token IF ELSE WHILE
%token CLASS ATTRIBUTE NEW POINT
%token EOF

%nonassoc EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD

%start program
%type <Kawa.program> program

%%

program:
| global_decls class_decls MAIN BEGIN instruction_seq END EOF
    { { globals = $1; classes = $2; main = $5 } }
;

global_decls:
| { [] }
| global_decl global_decls { $1 :: $2 }
;

global_decl:
| VAR IDENT IDENT SEMI { ($3, Kawa.typ_of_string $2) }
;

class_decls:
| { [] }
| class_decl class_decls { $1 :: $2 }
;

class_decl:
| CLASS IDENT BEGIN attribute_decls END
    { { class_name = $2; attributes = $4; methods = []; parent = None } }
;

attribute_decls:
| { [] }
| attribute_decl attribute_decls { $1 :: $2 }
;

attribute_decl:
| ATTRIBUTE IDENT IDENT SEMI { ($3, Kawa.typ_of_string $2) }
;

instruction:
| PRINT LPAR expression RPAR SEMI { Print($3) }
| IDENT AFFECT expression SEMI { Set(Var($1), $3) }
| WHILE expression BEGIN instruction_seq END { While($2, $4) }
// | IF expression BEGIN instruction_seq END { If($2, $4, []) }
| IF expression BEGIN instruction_seq END ELSE BEGIN instruction_seq END { If($2, $4, $8) }
// | lhs=expression AFFECT e=expression SEMI { Set(Expr(lhs), e) }
;

instruction_seq:
| { [] }
| instruction instruction_seq { $1 :: $2 }
;

expression:
| INT { Int($1) }
| BOOL { Bool($1) }
| NEW IDENT { New($2) }
| LPAR expression RPAR { $2 }
| expression EQ expression { Binop(Eq, $1, $3) }
| expression NEQ expression { Binop(Neq, $1, $3) }
| expression PLUS expression { Binop(Add, $1, $3) }
| expression MINUS expression { Binop(Sub, $1, $3) }
| expression TIMES expression { Binop(Mul, $1, $3) }
| expression DIV expression { Binop(Div, $1, $3) }
| expression MOD expression { Binop(Rem, $1, $3) }
;
