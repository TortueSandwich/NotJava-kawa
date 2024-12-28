%{

  open Lexing
  open Kawa

%}

%token MAIN BEGIN END EOF

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PRINT VAR IF ELSE WHILE CLASS ATTRIBUTE METHOD NEW THIS RETURN EXTENDS
%token AFFECT LPAR RPAR SEMI
%token PLUS MINUS TIMES DIV MOD 
%token LT LEQ GT GEQ AND OR EQ NEQ
%token POINT COMA 
%token EXCLAMATION
%token TINT TBOOL TVOID

%left OR
%left AND
%nonassoc EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right UNARY_OP
%left POINT


%start program
%type <Kawa.program> program


%%

program:
  globals=list(var_decl)
  classes=list(class_def)
  MAIN BEGIN 
  i=list(instruction)
  END EOF
  {
    let globals = List.flatten globals in
    { globals; classes; main=i; } 
  }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI {Print(e)}
| m=lvalue AFFECT e=expression SEMI {Set(m, e)}
| IF LPAR e=expression RPAR BEGIN iif=list(instruction) END ELSE BEGIN ielse=list(instruction) END {If(e, iif, ielse)}
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END {While(e,i)}
| RETURN e=expression SEMI {Return(e)}
| e=expression SEMI {Expr(e)}
;


lvalue:
| s=IDENT {Var(s)}
| e=lvalue POINT s=IDENT {Field(Get(e),s)}
;

expression:
| n=INT { {annot = TInt ; expr = Int(n) }}
| b=BOOL { {annot = TBool ; expr = Bool(b) }}
| t=THIS {{annot  =TVoid ; expr=This }}
| m=mem {{annot = TVoid ; expr = Get(m)}}
| o=unop e=expression %prec UNARY_OP {{annot = TVoid ; expr = Unop(o, e)}}
| e=expression o=binop f=expression { {annot = TVoid ; expr = Binop(o,e,f)} }
| LPAR e=expression RPAR { e }
| NEW i=IDENT {New(i)}
| NEW i=IDENT LPAR l=separated_list(COMA,expression) RPAR {NewCstr(i, l)}
| e=expression POINT s=IDENT LPAR l=separated_list(COMA,expression) RPAR {MethCall(e,s,l)}
;

%inline unop:
| MINUS {Opp}
| EXCLAMATION {Not}
| LPAR t=kawatype RPAR { TypeCast(t) } 
;


%inline mem:
| s=IDENT {Var(s) }
| e=expression POINT s=IDENT {Field(e,s)}
;


%inline kawatype:
| TINT {TInt}
| TBOOL {TBool}
| TVOID {TVoid}
| s=IDENT {TClass(s)}
;



class_def: 
| CLASS class_name=IDENT parent=extends? BEGIN attributes=list(attr_decl) methods=list(method_def) END {
   { class_name; attributes; methods; parent } 
}
;

extends : 
| EXTENDS parent_name=IDENT {parent_name}
;

var_decl:
| VAR t=kawatype l=separated_nonempty_list(COMA, IDENT) SEMI {List.map (fun x -> (x,t)) l}
;

attr_decl:
| ATTRIBUTE t=kawatype s=IDENT SEMI {(s,t)}


param:
| t=kawatype name=IDENT { (name, t) }

method_def: 
| METHOD return=kawatype method_name=IDENT LPAR params=separated_list(COMA,param) RPAR BEGIN locals=list(var_decl) code=list(instruction) END
{
  let locals = List.flatten locals in
  { method_name; code; params; locals; return;}}
;

param_decl : 
  t=IDENT name=IDENT {
    (name, Kawa.typ_of_string t)
  }
;

kawatype:
| TINT {TInt}
| TBOOL {TBool}
| TVOID {TVoid}
| s=IDENT {TClass(s)}
;

%inline unop:
| MINUS {Opp}
| EXCLAMATION {Not}
// | LPAR t=kawatype RPAR { TypeCast(t) } 
;

<<<<<<<<< Temporary merge branch 1

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



| LPAR s=IDENT RPAR expression { Unop(TypeCast(typ_of_string(s)),$4)}

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
=========
%inline binop:
| PLUS {Add}
| MINUS {Sub}
| TIMES {Mul}
| DIV {Div}
| MOD {Rem}
| LT {Lt}
| LEQ {Le}
| GT {Gt}
| GEQ {Ge}
| EQ {Eq}
| NEQ {Neq}
| AND {And}
| OR {Or}
;

