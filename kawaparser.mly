%{

  open Lexing
  open Kawa

  exception ParserError of string


  let rec tarray_of_dim n t = if n = 0 then t else TArray(tarray_of_dim (n-1) t) 
%}

%token MAIN BEGIN END EOF
%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PRINT VAR IF ELSE WHILE CLASS INTERFACE ATTRIBUTE METHOD NEW THIS RETURN EXTENDS IMPLEMENTS DEFAULT SUPER
%token AFFECT LPAR RPAR SEMI LBR RBR
%token PLUS MINUS TIMES DIV MOD
%token LT LEQ GT GEQ AND OR EQ NEQ STRUCTEQ NEGSTRUCTEQ
%token POINT COMA EXCLAMATION
%token TINT TBOOL TVOID
%token INSTANCEOF AS
%token FINAL GENERIC
%token PUBLIC PRIVATE PROTECTED


%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc STRUCTEQ NEGSTRUCTEQ
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
  interfaces=list(interface_def)
  classes=list(class_def)
  MAIN BEGIN 
  main_instrs=list(instruction)
  end_handled EOF
  {
    (* traite le cas où les variables sont initialisé à la déclaration *)
    let (globals, globals_init) = 
      List.fold_left 
        (fun (acc_globals, acc_instrs) (defs, instrs) -> 
          (acc_globals @ defs, acc_instrs @ instrs)
        ) ([], []) globals_var
    in
    { globals; interfaces ;classes; main = globals_init @ main_instrs }
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
| instrs=raw_instruction { {instr=instrs;loc=$loc}}

raw_instruction:
| PRINT LPAR e=expression rpar_handled semi_handled {  Print(e) }
| m=mem AFFECT e=expression semi_handled { Set(m, e) }
| IF LPAR e=expression rpar_handled BEGIN iif=list(instruction) end_handled ELSE BEGIN ielse=list(instruction) end_handled { If(e, iif, ielse) }
| WHILE LPAR e=expression rpar_handled BEGIN i=list(instruction) end_handled { While(e,i) }
| RETURN e=expression semi_handled { Return(e) }
| e=expression semi_handled { Expr(e) }
| BEGIN l=list(instruction) end_handled { Scope(l) }
| x=var_decl { x }
;

var_decl:
| VAR t=kawatype vars=separated_nonempty_list(COMA, IDENT) value=affectation? semi_handled { Declare(vars, t, value) }
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
| expr=raw_expression { let (t, e) = expr in {annot=t;expr=e;loc= $loc}}

raw_expression:
| n=INT { (TInt, Int(n) ) }
| b=BOOL { (TBool, Bool(b) ) }
| t=THIS {(TVoid,This)}
| m=mem { (TVoid, Get(m)) }
| o=unop e=expression %prec UNARY_OP {(TVoid, Unop(o, e) )}
| e=expression o=binop f=expression { (TVoid, Binop(o,e,f) )} 
| LPAR e=raw_expression rpar_handled { e }
| NEW i=IDENT {(TClass(i, []),  New(i, []) )}
| NEW GENERIC i=IDENT gt=generictype {(TClass(i, gt),  New(i, gt) )}
| NEW i=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {(TClass(i, []), NewCstr(i, [], l) )}
| NEW GENERIC i=IDENT gt=generictype LPAR l=separated_list(COMA,expression) rpar_handled {(TClass(i, gt), NewCstr(i, gt, l) )}
| e=expression POINT s=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {(TVoid, MethCall(e,s,l) )}
| SUPER POINT s=IDENT LPAR l=separated_list(COMA,expression) rpar_handled {(TVoid, SuperCall(s,l) )}
| e=expression AS t=kawatype {(TVoid, Unop(TypeCast(t), e) )}
| e=expression INSTANCEOF t=kawatype {  (TBool, Unop(InstanceOf(t) , e) )}  
| NEW t=base_types d=nonempty_list(dimension) { (tarray_of_dim (List.length d) t, NewArray(t, d) )} 
;

dimension:
|  LBR e=expression RBR { e } 
;


affectation:
| AFFECT e=expression {e}
;

class_def: 
| CLASS class_name=IDENT 
generics=gen_args?
 parent=extends? implemented_interfaces=implements BEGIN attributes=list(attr_decl) methods=list(method_def) end_handled {
  let generics : string list = (Option.value ~default:[] generics) in
   { class_name; generics; attributes; methods; parent; implemented_interfaces}  
}
;

gen_args :
| LT t=separated_list(COMA,IDENT) GT {t}


extends : 
| EXTENDS parent_name=IDENT {parent_name}
;

implements :
| IMPLEMENTS interfaces=separated_nonempty_list(COMA, IDENT) {interfaces}
| { [] }
;

interface_def: 
| INTERFACE interface_name=IDENT BEGIN methods=list(method_def_inter) end_handled {
   { interface_name; methods} 
}
;

final:
| f=FINAL? {Option.is_none f}


attr_decl:
| f=final ATTRIBUTE t=kawatype s=IDENT semi_handled {(s,t, Public, f)}
| PUBLIC f=final ATTRIBUTE t=kawatype s=IDENT semi_handled {(s,t, Public,f)}
| PRIVATE f=final ATTRIBUTE t=kawatype s=IDENT semi_handled {(s,t, Private,f)}
| PROTECTED f=final ATTRIBUTE t=kawatype s=IDENT semi_handled {(s,t, Protected,f)}
;


param:
| t=kawatype name=IDENT { (name, t) }
;

method_def: 
| METHOD return=kawatype method_name=IDENT LPAR params=separated_list(COMA,param) rpar_handled BEGIN 
 code=list(instruction) end_handled
{
  { method_name; code; params; locals=[]; return; default = true}}
;


method_def_inter:
| d=def_avec_default {d}
| d=def_sans_default {d}
;


def_avec_default: 
| DEFAULT METHOD return=kawatype method_name=IDENT LPAR params=separated_list(COMA,param) rpar_handled BEGIN 
 code=list(instruction) end_handled
{
  { method_name; code; params; locals=[]; return; default = true }}
;

def_sans_default: 
| METHOD return=kawatype method_name=IDENT LPAR params=separated_list(COMA,param) rpar_handled semi_handled
{
  { method_name; code = [] ; params; locals=[]; return; default = false}}
;

mem:
| m=memsimple {m}
| s=memsimple e=nonempty_list(dimension) {Array_var(s,e)}
;

memsimple:
| s=IDENT {Var(s) }
| e=expression POINT s=IDENT {Field(e,s)}
;

%inline base_types:
| TINT {TInt}
| TBOOL {TBool}
| TVOID {TVoid}
| s=IDENT {TClass(s,[])}
| GENERIC s=IDENT t=generictype {TClass(s,t)} 
;

generictype : 
| BEGIN t=separated_list(COMA,kawatype) END {t}


%inline kawatype:
| t=base_types { t }
| t=base_types dim=nonempty_list(bracket_pair)  { tarray_of_dim (List.length dim) t }
;

bracket_pair:
| LBR RBR { 1 }
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
| STRUCTEQ {StructEq}
| NEGSTRUCTEQ {NegStructEq}

;