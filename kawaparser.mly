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
%token INSTANCEOF, AS

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

%%

program:
  globals_var=list(globals_var_decl)
  classes=list(class_def)
  MAIN BEGIN 
  i=list(instruction)
  end_handled EOF
  {
    let (globals, globals_init) = 
      List.fold_left 
        (fun (acc_globals, acc_instrs) (defs, instrs) -> 
          (acc_globals @ defs, acc_instrs @ instrs)
        ) ([], []) globals_var
    in
    { globals; classes; main = globals_init @ i }
  }
;

globals_var_decl:
| VAR t=kawatype vars=separated_nonempty_list(COMA, IDENT) value=affectation? semi_handled { 
  let defs = List.map (fun var -> (var, t)) vars in
  let instrs = match value with 
  | None -> []
  | Some value -> List.map (fun var -> Set(Var(var), value)) vars in
  (defs, instrs)
  
}
;

instruction:
| PRINT LPAR e=expression rpar_handled semi_handled {Print(e)}
| m=mem AFFECT e=expression semi_handled {Set(m, e)}
| IF LPAR e=expression rpar_handled BEGIN iif=list(instruction) end_handled ELSE BEGIN ielse=list(instruction) end_handled {If(e, iif, ielse)}
| WHILE LPAR e=expression rpar_handled BEGIN i=list(instruction) end_handled {While(e,i)}
| RETURN e=expression semi_handled {Return(e)}
| e=expression semi_handled {Expr(e)}
| BEGIN l=list(instruction) end_handled {Scope(l)}
| x=var_decl { let (a,b,c) = x in Declare(a,b,c) }
;

semi_handled : 
| error {raise (ParserError("point-virgule manquant"))}
| SEMI { () }


end_handled : 
| error {raise (ParserError("une accolade n'est pas fermee"))}
| END { () }


rpar_handled : 
| error {raise (ParserError("une paranthese n'est pas fermee"))}
| RPAR { () }



expression:
| n=INT { {annot = TInt ; expr = Int(n) }}
| b=BOOL { {annot = TBool ; expr = Bool(b) }}
| t=THIS {{annot  =TVoid ; expr=This }}
| m=mem {{annot = TVoid ; expr = Get(m)}}
| o=unop e=expression %prec UNARY_OP {{annot = TVoid ; expr = Unop(o, e)}}
| e=expression o=binop f=expression { {annot = TVoid ; expr = Binop(o,e,f)} }
| LPAR e=expression rpar_handled { e }
| NEW i=IDENT {{annot = TClass(i) ; expr =  New(i)}}
| NEW i=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {{annot = TClass(i) ; expr = NewCstr(i, l)}}
| e=expression POINT s=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {{annot = TVoid ; expr = MethCall(e,s,l)}}
| e=expression AS t=kawatype {{annot = TVoid ; expr = Unop(TypeCast(t), e)}}
| e=expression INSTANCEOF t=kawatype {  {annot = TBool ; expr = Unop(InstanceOf(t) , e)}  }
;

var_decl:
| VAR t=kawatype vars=separated_nonempty_list(COMA, IDENT) value=affectation? semi_handled { (vars, t, value) }
;

affectation:
| AFFECT e=expression {e}
;

class_def: 
| CLASS class_name=IDENT parent=extends? BEGIN attributes=list(attr_decl) methods=list(method_def) end_handled {
   { class_name; attributes; methods; parent } 
}
;

extends : 
| EXTENDS parent_name=IDENT {parent_name}
;


attr_decl:
| ATTRIBUTE t=kawatype s=IDENT semi_handled {(s,t)}
;

param:
| t=kawatype name=IDENT { (name, t) }
;

method_def: 
| METHOD return=kawatype method_name=IDENT LPAR params=separated_list(COMA,param) rpar_handled BEGIN 
 code=list(instruction) end_handled
{
  { method_name; code; params; locals=[]; return;}}
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

%inline unop:
| MINUS {Opp}
| EXCLAMATION {Not}
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
