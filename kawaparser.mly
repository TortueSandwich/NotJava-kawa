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


%type <string list> var_decl_aux
%type <((string*typ) * (expr option)) list> var_decl
%%

program:
  list(var_decl) 
  list(class_decl) 
  MAIN BEGIN 
    list(instruction) 
  END EOF
  { 
    let okayyy = List.flatten $1 in
    let (oue, _) = List.split okayyy in
    let truevals = List.filter (fun (_, opt) -> Option.is_some opt) okayyy in
    let instrs = List.map (fun (x, value) -> Set(Var(fst x), Option.get value)) truevals in
    { globals = oue; classes = $2; main = instrs @ $5 } 
  }
;


var_decl:
| VAR typ=IDENT vars=var_decl_aux SEMI { List.map (fun v -> 
  (( (v, Kawa.typ_of_string typ) , None))
) vars }
| VAR typ=IDENT vars=var_decl_aux AFFECT 
  value=expression SEMI
{
  List.map (fun v ->
  ((v, Kawa.typ_of_string typ), Some value)
) vars 
}
;

var_decl_aux:
| id=IDENT {id :: []}
| id=IDENT COMA ids=var_decl_aux {id :: ids}
;

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
  METHOD ret=IDENT method_name=IDENT LPAR params=separated_list(COMA, param_decl) RPAR BEGIN 
  vars=list(var_decl)
  code=list(instruction)
  END
  { 
    let okayyy = List.flatten vars in
    let (oue, _) = List.split okayyy in
    let truevals = List.filter (fun (_, opt) -> Option.is_some opt) okayyy in
    let instrs = List.map (fun (x, value) -> Set(Var(fst x), Option.get value)) truevals in
    let code = instrs @ code in
    { method_name; code; params; locals=oue; return=Kawa.typ_of_string ret;}
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

// | e=expression POINT meth_name=IDENT LPAR l=separated_list(COMA, expression) RPAR { MethCall(e, meth_name, l) }
| e=expression POINT meth_name=IDENT LPAR RPAR { MethCall(e, meth_name, []) }

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
;

