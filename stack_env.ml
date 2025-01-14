type env_error =
  | UndefinedVariable of string
  | AlreadyDeclared of string
  | EmptyEnvStack

exception EnvError of env_error

let envraise x = EnvError x |> raise

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

  let get_all_names (env_stack : env_stack) : string list =
    let names_set = Hashtbl.create 16 in
    List.iter
      (fun env ->
        Hashtbl.iter (fun key _value -> Hashtbl.replace names_set key ()) env)
      env_stack;
    Hashtbl.fold (fun key _acc lst -> key :: lst) names_set []

  (* Référence pour l'environnement global unique *)
  let global_env_ref = ref (Hashtbl.create 16)
  let get_global_env () : env = !global_env_ref

  (* Manipulation de la Stack*)

  let new_env_stack () : env_stack = [ !global_env_ref ]

  (* Ajoute un nouvel environnement local à la pile *)
  let new_env env_stack : env_stack = Hashtbl.create 16 :: env_stack

  (* Retire l'environnement local le plus récent *)
  let pop_local_env = function
    | [] -> failwith "Impossible de dépiler : aucun environnement"
    | _ :: rest -> rest

  (* find *)
      
  (** @raise UndefinedVariable *)
  let rec find env_stack key =
    match env_stack with
    | [] -> UndefinedVariable key |> envraise
    | env :: _ when Hashtbl.mem env key -> Hashtbl.find env key
    | _ :: rest -> find rest key

  (** @raise UndefinedVariable *)
  let find_locally env_stack key =
    match env_stack with
    | [] -> EmptyEnvStack |> envraise
    | env :: _ ->
        if Hashtbl.mem env key then Hashtbl.find env key
        else UndefinedVariable key |> envraise

  (* define *)

  (** Définit une variable localement dans l'environnement local courant
      @raise AlreadyDeclared *)
  let define_locally env_stack key value =
    match env_stack with
    | [] -> EmptyEnvStack |> envraise
    | local_env :: _ ->
        if Hashtbl.mem local_env key then AlreadyDeclared key |> envraise
        else Hashtbl.add local_env key value

  (** Déclare une variable globalement (utilise l'environnement global unique)
      @raise AlreadyDeclared *)
  let define_globally key value =
    let global_env = !global_env_ref in
    if Hashtbl.mem global_env key then AlreadyDeclared key |> envraise
    else Hashtbl.add global_env key value

  (* Replace *)

  (** @raise UndefinedVariable *)
  let replace_locally env_stack key value =
    match env_stack with
    | [] -> EmptyEnvStack |> envraise
    | env :: rest ->
        if Hashtbl.mem env key then Hashtbl.replace env key value
        else UndefinedVariable key |> envraise

  (** @raise UndefinedVariable *)
  let rec replace env_stack key value =
    match env_stack with
    | [] -> UndefinedVariable key |> envraise
    | env :: rest ->
        if Hashtbl.mem env key then Hashtbl.replace env key value
        else replace rest key value

  (* is declared *)

  (* Vérifie si une variable est déclarée localement dans l'environnement actif *)
  let is_declared_locally env_stack key =
    match env_stack with
    | [] -> EmptyEnvStack |> envraise
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
              Printf.printf "%s\"%s\": %s\n" indent key
                (V.string_of_value value))
            current_env;
          print_env rest (indent ^ "  ")
    in
    print_string "{\n";
    print_env env_stack "";
    print_string "}\n"
end
