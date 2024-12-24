%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token MAIN PRINT BEGIN END VAR IF ELSE WHILE CLASS ATTRIBUTE METHOD NEW THIS RETURN EXTENDS
%token AFFECT LPAR RPAR SEMI
%token PLUS MINUS TIMES DIV MOD 
%token LT LEQ GT GEQ AND OR EQ NEQ
%token POINT COMA 
%token EOF

%left OR
%left AND
%nonassoc EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right AFFECT
%left POINT



%start program
%type <Kawa.program> program

%%

program:
  list(global_decl) 
  list(class_decl) 
  MAIN BEGIN 
    list(instruction) 
  END EOF
  { { globals = List.flatten $1; classes = $2; main = $5 } }
;


global_decl:
| VAR typ=IDENT 
vars=glob_de_aux
SEMI { 
  let x = List.map (fun v -> (v,Kawa.typ_of_string typ)) vars in
  x
  }
;

glob_de_aux:
| id=IDENT {id :: []}
| id=IDENT COMA ids=glob_de_aux {id :: ids}

class_decl:
  CLASS class_name=IDENT parent=extend?
  BEGIN
    attributes=list(attribute_decl) 
    methods=list(method_def)
  END
    { { class_name; attributes; methods; parent } }
;

extend : 
  EXTENDS parent_name=IDENT {parent_name}
;

attribute_decl:
| ATTRIBUTE IDENT IDENT SEMI { ($3, Kawa.typ_of_string $2) }
;

method_def:
  METHOD t=IDENT method_name=IDENT LPAR params=separated_list(COMA, param_decl) RPAR BEGIN 
  code=list(instruction)
  END
  { { method_name; code; params; locals=[]; return=Kawa.typ_of_string t;
  }
  }
;

param_decl : 
  t=IDENT name=IDENT {
    (name, Kawa.typ_of_string t)
  }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| name=mem AFFECT e=expression SEMI { Set(name, e) }
| WHILE cond=expression BEGIN 
    instrs=list(instruction) 
  END 
  { While(cond, instrs) }
| IF cond=expression BEGIN eif=list(instruction) END
  ELSE BEGIN eelse=list(instruction) END 
  { If(cond, eif, eelse) }
| RETURN e=expression SEMI { Return(e) }
;

mem:
| name=IDENT { Var(name) }
| THIS POINT field=IDENT { Field(This, field) }
| mem POINT field=IDENT { Field(Get($1), field) }
;


expression:
| INT { Int($1) }
| BOOL { Bool($1) }
| NEW class_name=IDENT { New(class_name) }
| NEW class_name=IDENT LPAR l=separated_list(COMA,expression) RPAR { NewCstr(class_name, l) }
| e=expression POINT meth_name=IDENT LPAR l=separated_list(COMA, expression) RPAR { MethCall(e, meth_name, l) }
| name=IDENT { Get(Var(name)) }
| THIS POINT field=IDENT { Get(Field(This, field)) }
| e=expression POINT field=IDENT { Get(Field(e, field)) }

// todo uop
| LPAR expression RPAR { $2 }
| expression EQ expression { Binop(Eq, $1, $3) }
| expression NEQ expression { Binop(Neq, $1, $3) }

| expression LT expression { Binop(Lt, $1, $3) }
| expression LEQ expression { Binop(Le, $1, $3) }
| expression GT expression { Binop(Gt, $1, $3) }
| expression GEQ expression { Binop(Ge, $1, $3) }
| expression AND expression { Binop(And, $1, $3) }
| expression OR expression { Binop(Or, $1, $3) }

| expression PLUS expression { Binop(Add, $1, $3) }
| expression MINUS expression { Binop(Sub, $1, $3) }
| expression TIMES expression { Binop(Mul, $1, $3) }
| expression DIV expression { Binop(Div, $1, $3) }
| expression MOD expression { Binop(Rem, $1, $3) }
;

