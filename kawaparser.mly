%{

  open Lexing
  open Kawa

  exception ParserError of string
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
%token INSTANCEOF

%token AS

%left OR
%left AND
%nonassoc EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right UNARY_OP
%left POINT
%left INSTANCEOF
%left AS 

%start program
%type <Kawa.program> program
// %type <Kawa.mem_access> lvalue



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
| PRINT LPAR e=expression RPAR check_semi {Print(e)}
| m=mem AFFECT e=expression check_semi {Set(m, e)}
| IF LPAR e=expression RPAR BEGIN iif=list(instruction) END ELSE BEGIN ielse=list(instruction) END {If(e, iif, ielse)}
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END {While(e,i)}
| RETURN e=expression check_semi {Return(e)}
| e=expression check_semi {Expr(e)}
;

check_semi:
| SEMI { () }
| error { raise (ParserError "Missing semicolon") }
;


expression:
| n=INT { {annot = TInt ; expr = Int(n) }}
| b=BOOL { {annot = TBool ; expr = Bool(b) }}
| t=THIS {{annot  =TVoid ; expr=This }}
| m=mem {{annot = TVoid ; expr = Get(m)}}
| o=unop e=expression %prec UNARY_OP {{annot = TVoid ; expr = Unop(o, e)}}
| e=expression o=binop f=expression { {annot = TVoid ; expr = Binop(o,e,f)} }
| LPAR e=expression RPAR { e }
| NEW i=IDENT {{annot = TClass(i) ; expr =  New(i)}}
| NEW i=IDENT LPAR l=separated_list(COMA,expression) RPAR {{annot = TClass(i) ; expr = NewCstr(i, l)}}
| e=expression POINT s=IDENT LPAR l=separated_list(COMA,expression) RPAR {{annot = TVoid ; expr = MethCall(e,s,l)}}
| e=expression AS t=kawatype {{annot = TVoid ; expr = Unop(TypeCast(t), e)}}
| e=expression INSTANCEOF t=kawatype {  {annot = TBool ; expr = Unop(InstanceOf(t) , e)}  }
;

%inline unop:
| MINUS {Opp}
| EXCLAMATION {Not}
//| LPAR t=kawatype RPAR { TypeCast(t) } 
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
