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

%token GENERIC

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
  | Some value -> List.map (fun var -> {instr = Set(Var(var), value); loc = $loc}) vars in
  (defs, instrs)
  
}
;

instruction:
| PRINT LPAR e=expression rpar_handled semi_handled { {instr = Print(e); loc = $loc }}
| m=mem AFFECT e=expression semi_handled {{instr = Set(m, e); loc = $loc }}
| IF LPAR e=expression rpar_handled BEGIN iif=list(instruction) end_handled ELSE BEGIN ielse=list(instruction) end_handled {{instr = If(e, iif, ielse); loc = $loc }}
| WHILE LPAR e=expression rpar_handled BEGIN i=list(instruction) end_handled {{instr = While(e,i); loc = $loc }}
| RETURN e=expression semi_handled {{instr = Return(e); loc = $loc }}
| e=expression semi_handled {{instr = Expr(e); loc = $loc }}
| BEGIN l=list(instruction) end_handled {{instr = Scope(l); loc = $loc }}
| x=var_decl { let (a,b,c) = x in {instr = Declare(a,b,c); loc = $loc  }}
;

var_decl:
| VAR t=kawatype vars=separated_nonempty_list(COMA, IDENT) value=affectation? semi_handled { (vars, t, value) }
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
| n=INT { {annot = TInt ; expr = Int(n) ; loc = $loc }}
| b=BOOL { {annot = TBool ; expr = Bool(b) ; loc = $loc }}
| t=THIS {{annot  =TVoid ; expr=This ; loc = $loc}}
| m=mem {{annot = TVoid ; expr = Get(m); loc = $loc}}
| o=unop e=expression %prec UNARY_OP {{annot = TVoid ; expr = Unop(o, e) ; loc = $loc}}
| e=expression o=binop f=expression { {annot = TVoid ; expr = Binop(o,e,f) ; loc = $loc} }
| LPAR e=expression rpar_handled { e }
| NEW i=IDENT {{annot = TClass(i, []) ; expr =  New(i, []) ; loc = $loc}}
| NEW GENERIC i=IDENT gt=generictype {{annot = TClass(i, gt) ; expr =  New(i, gt) ; loc = $loc}}
| NEW i=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {{annot = TClass(i, []) ; expr = NewCstr(i, [], l) ; loc = $loc}}
| NEW GENERIC i=IDENT gt=generictype LPAR l=separated_list(COMA,expression) rpar_handled {{annot = TClass(i, gt) ; expr = NewCstr(i, gt, l) ; loc = $loc}}
| e=expression POINT s=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {{annot = TVoid ; expr = MethCall(e,s,l) ; loc = $loc}}
| e=expression AS t=kawatype {{annot = TVoid ; expr = Unop(TypeCast(t), e) ; loc = $loc}}
| e=expression INSTANCEOF t=kawatype {  {annot = TBool ; expr = Unop(InstanceOf(t) , e) ; loc = $loc}  }
;

affectation:
| AFFECT e=expression {e}
;

class_def: 
| CLASS class_name=IDENT 
generics=gen_args?
 parent=extends? BEGIN attributes=list(attr_decl) methods=list(method_def) end_handled {
  let generics : string list = (Option.value ~default:[] generics) in
   { class_name; generics; attributes; methods; parent } 
}
;

gen_args :
| LT t=separated_list(COMA,IDENT) GT {t}


extends : 
| EXTENDS parent_name=IDENT {parent_name}
;


attr_decl:
| ATTRIBUTE t=kawatype s=IDENT semi_handled {
  (s,t)
  }
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
| s=IDENT {TClass(s,[])}
| GENERIC s=IDENT t=generictype {TClass(s,t)} 
;

generictype : 
| BEGIN t=separated_list(COMA,kawatype) END {t}


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
