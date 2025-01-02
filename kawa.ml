(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les
   paramètres et résultats des méthodes. *)
type typ = TVoid | TInt | TBool | TClass of string

let typ_of_string = function
  | "int" -> TInt
  | "bool" -> TBool
  | "void" -> TVoid
  | classname -> TClass classname

let typ_to_string = function
  | TVoid -> "void"
  | TInt -> "int"
  | TBool -> "bool"
  | TClass c -> c


type unop = Opp | Not | TypeCast of typ | InstanceOf of typ

type binop =
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

(* Expressions *)
type expr = {annot : typ ; expr : expr_}
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
  | New of string
  | NewCstr of string * expr list
  (* Appel de méthode *)
  | MethCall of expr * string * expr list

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var of string (* Variable *)
  | Field of expr (* objet *) * string (* nom d'un attribut *)

(* Instructions *)
type instr =
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
}

(* Définition de classe

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du
   paramètre implicite this. *)
type class_def = {
  class_name : string;
  attributes : (string * typ) list;
  methods : method_def list;
  parent : string option;
}

(* Programme complet : variables globales, classes, et une séquence
   d'instructions *)
type program = {
  classes : class_def list;
  globals : (string * typ) list;
  main : seq;
}

let string_of_unop unop = match unop with 
  Opp -> "Opp" | Not -> "Not" 
  | TypeCast (newType) -> "TypeCast("^( typ_to_string newType) ^ ")"
  | InstanceOf (t) -> "InstanceOf ("^( typ_to_string t) ^ ")"

let string_of_biop (biop : binop) : string =
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
  | Get m -> "Get(" ^ (string_of_meme m) ^")"
  | This -> "This"
  | New c -> fmt "%s" c
  | NewCstr (c, _) -> fmt "%s" c
  | MethCall (e1, c, el) -> fmt "%s" c
and string_of_meme = function
    | Var name -> name
    | Field (obj, field_name) -> (
      (string_of_expr obj) ^ "." ^ field_name
    )

