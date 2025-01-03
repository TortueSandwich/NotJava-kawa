type env_error =
  | UndefinedVariable of string 
  | AlreadyDeclared of string   
  | EmptyEnvStack

exception EnvError of env_error

let string_of_env_error = function
| UndefinedVariable s -> "Undefined variable : " ^ s
| AlreadyDeclared s -> "Variable : " ^ s ^ " is already declared"
| EmptyEnvStack -> "No environement"

module type ValueType = sig
  type t
  val string_of_value : t -> string
end

module MakeEnv (V : ValueType) = struct
  type value = V.t
  type env = (string, value) Hashtbl.t
  type env_stack = env list
  
  
  (* Référence pour l'environnement global unique *)
  let global_env_ref = ref (Hashtbl.create 16)

  let get_global_env () : env = !global_env_ref

  (* Manipulation de la Stack*)

  let new_env_stack () : env_stack =
    [!global_env_ref]

  (* Ajoute un nouvel environnement local à la pile *)
  let new_env env_stack : env_stack =
    Hashtbl.create 16 :: env_stack

  (* Retire l'environnement local le plus récent *)
  let pop_local_env = function
    | [] -> failwith "Impossible de dépiler : aucun environnement"
    | _ :: rest -> rest


  (* find *)

  let rec find env_stack key =
      match env_stack with
      | [] -> raise (EnvError (UndefinedVariable key))
      | env :: _ when Hashtbl.mem env key -> Hashtbl.find env key
      | _ :: rest -> find rest key

  let find_locally env_stack key =
    match env_stack with
    | [] -> raise (EnvError EmptyEnvStack)
    | env :: _ ->
        if Hashtbl.mem env key then
          Hashtbl.find env key
        else
          raise (EnvError (UndefinedVariable key))

  (* define *)        

  (* Définit une variable localement dans l'environnement local courant *)
  let define_locally env_stack key value =
    match env_stack with
    | [] -> raise (EnvError EmptyEnvStack)
    | local_env :: _ ->
        if Hashtbl.mem local_env key then
          raise (EnvError (AlreadyDeclared key))
        else
          Hashtbl.add local_env key value

  (* Déclare une variable globalement (utilise l'environnement global unique) *)
  let define_globally key value =
    let global_env = !global_env_ref in
    if Hashtbl.mem global_env key then
      raise (EnvError (AlreadyDeclared key))
    else
      Hashtbl.add global_env key value

  (* Replace *)

  let replace_locally env_stack key value =
    match env_stack with
    | [] -> raise (EnvError EmptyEnvStack)
    | env :: rest ->
        if Hashtbl.mem env key then
          Hashtbl.replace env key value 
        else
          raise (EnvError (UndefinedVariable key)) 

  let rec replace env_stack key value =
    match env_stack with
    | [] -> raise (EnvError (UndefinedVariable key))
    | env :: rest ->
        if Hashtbl.mem env key then
          Hashtbl.replace env key value 
        else
          replace rest key value 

  (* is declared *)

  (* Vérifie si une variable est déclarée localement dans l'environnement actif *)
  let is_declared_locally env_stack key =
    match env_stack with
    | [] -> raise (EnvError EmptyEnvStack)
    | local_env :: _ -> Hashtbl.mem local_env key 

  let rec is_declared env_stack key =
    match env_stack with
    | [] -> false
    | local_env :: rest ->
        if Hashtbl.mem local_env key then true else is_declared rest key


  (* Print *)

  let print_env_stack env_stack =
    let rec print_env env_stack indent =
      match env_stack with
      | [] -> ()
      | current_env :: rest ->
          Hashtbl.iter
            (fun key value ->
              Printf.printf "%s\"%s\": %s\n" indent key (V.string_of_value value))
            current_env;
          print_env rest (indent ^ "  ")
    in
    print_string "{\n";
    print_env env_stack "";
    print_string "}\n"
end
