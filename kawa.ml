(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les
   paramètres et résultats des méthodes. *)

type typ = TVoid | TInt | TBool | TClass of string * typ list | TArray of typ

let rec typ_of_string = function
  | "int" -> TInt
  | "bool" -> TBool
  | "void" -> TVoid
  | s->if Tools.is_valid_array_string s then TArray (typ_of_string (String.sub s 0 ((String.length s) - 2))) else TClass (s,[])

let rec string_of_typ = function
  | TVoid -> "void"
  | TInt -> "int"
  | TBool -> "bool"
  | TClass(c,l) -> c ^ "{" ^ 
  (* :if Option.is_some l then string_of_typ (Option.get l) else "" *)
  List.fold_left (fun acc x -> acc  ^ (string_of_typ x) ^ ", ") "" l 
  ^ "}"
  | TArray t -> let rec aux t acc= 
                      match t with 
                      TArray t -> aux t ("[]"^acc) 
                      | base_type -> (string_of_typ base_type) ^ acc 
                      in aux t "[]" 


type unop = Opp | Not | TypeCast of typ | InstanceOf of typ 
(* Opérations binaires *)

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | And
  | Or
  | StructEq
  | NegStructEq

(* Expressions *)
and expr = {annot : typ ; expr : expr_; loc : Lexing.position * Lexing.position}
and expr_ = 
  (* Base arithmétique *)
  | Int of int
  | Bool of bool
  | Unop of unop * expr
  | Binop of binop * expr * expr
  (* Accès à une variable ou un attribut *)
  | Get of mem_access
  (* Objet courant *)
  | This
  (* Création d'un nouvel objet *)
  | New of string * typ list
  | NewCstr of string * typ list * expr list
  (* Appel de méthode *)
  | MethCall of expr * string * expr list
  (* Appel à super*)
  |SuperCall of string * expr list
  (* Création d'un tableau *)
  (*Création d'un tableau*)
  | NewArray of typ * expr list   (*dimensions*)

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var of string (* Variable *)
  | Field of expr (* objet *) * string (* nom d'un attribut *)
  | Array_var of string(* Variable de type array *) * expr list(* indice *)

(* Instructions *)
type instr = { instr : instr_ ;  loc : Lexing.position * Lexing.position}
and instr_ =
  (* Affichage d'un entier *)
  | Print of expr
  (* Écriture dans une variable ou un attribut *)
  | Set of mem_access * expr
  (* Structures de contrôle usuelles *)
  | If of expr * seq * seq
  | While of expr * seq
  (* Fin d'une fonction *)
  | Return of expr
  (* Expression utilisée comme instruction *)
  | Expr of expr
  | Scope of seq
  | Declare of string list * typ * expr option

and seq = instr list

(* Définition de méthode

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type method_def = {
  method_name : string;
  code : seq;
  params : (string * typ) list;
  locals : (string * typ) list;
  return : typ;
  default : bool;
}

(* Définition de classe

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du
   paramètre implicite this. *)
type class_def = {
  class_name : string;
  generics : string list;
  attributes : (string * typ) list;
  methods : method_def list;
  parent : string option;
  implemented_interfaces : string list;
}

(* Définition d'une interface

   Syntaxe : interface <nom de l'interface> { ... }

*)

type interface_def = {
  interface_name : string;
  methods : method_def list;
}

(* Programme complet : variables globales, classes, et une séquence
   d'instructions *)
type program = {
  interfaces : interface_def list;
  classes : class_def list;
  globals : (string * typ) list;
  main : seq;
}

let rec string_of_expr (e : expr) : string =
  let fmt = Printf.sprintf in
  match e.expr with
  | Int i -> fmt "Int(%d)" i
  | Bool b -> fmt "Bool(%b)" b
  | Unop (unop, expr) ->
      fmt "Unop(%s, %s)" (string_of_unop unop) (string_of_expr expr)
  | Binop (biop, e1, e2) ->
      fmt "Biop(%s, %s, %s)" (string_of_biop biop) (string_of_expr e1)
        (string_of_expr e2)
  | Get m -> "Get(" ^ (string_of_mem m) ^")"
  | This -> "This"
  | New (c, gen) -> fmt "%s" c
  | NewCstr (c,_ ,_) -> fmt "%s" c
  | MethCall (e1, c, el) -> (string_of_expr e1) ^ (fmt "%s" c) ^ "(...)"
  | NewArray (t, n) -> fmt "new %s%s" (string_of_typ t) (List.fold_left (fun acc x -> acc ^ "[" ^ (string_of_expr x) ^ "]") "" n)
  | SuperCall(c, el) -> fmt "super.%s" c
and string_of_unop unop = match unop with 
  Opp -> "Opp" | Not -> "Not" 
  | TypeCast (newType) -> "TypeCast("^( string_of_typ newType) ^ ")"
  | InstanceOf (t) -> "InstanceOf ("^( string_of_typ t) ^ ")"

and string_of_biop (biop : binop) : string =
    match biop with
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Div -> "Div"
    | Rem -> "Rem"
    | Lt -> "Lt"
    | Le -> "Le"
    | Gt -> "Gt"
    | Ge -> "Ge"
    | Eq -> "Eq"
    | Neq -> "Neq"
    | And -> "And"
    | Or -> "Or"
    | StructEq -> "StructEq"
    | NegStructEq -> "NegStructEq"
  

and string_of_mem = function
    | Var name -> name
    | Field (obj, field_name) -> (
      (string_of_expr obj) ^ "." ^ field_name
    )
    | Array_var (name, i) -> name ^ List.fold_left (fun acc x -> acc^"[" ^ (string_of_expr x) ^ "]" ) "" i
    

let string_of_instr = function
  | Print e -> "print"
  | Set (m,v) -> "set"
  | If (c,i,e) -> "if"
  | While (c,s)-> "while"
  | Return e -> "return"
  | Expr e -> "expr"
  | Scope s -> "scope"
  | Declare (v,t,value) -> "declare " ^ (List.fold_left (fun acc x -> acc ^ " "^ x) "" v)
