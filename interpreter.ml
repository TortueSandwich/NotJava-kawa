open Kawa
open Typechecker (*Pour importer check_subtype*)

type value =
  | Null
  | VInt of int
  | VBool of bool
  | VObj of obj
  | VArray of value array

and obj = { cls : string; fields : (string, value) Hashtbl.t }

type error =
  | DimensionMismatch of expr
  | NotFound
  | NotIndexable of value
  | InvalidIndex of expr * value
  | Division_by_zero of expr
  | Anomaly

exception IError of error

(* exception Error of string *)
exception Return of value

let iraise e = raise (IError e)

module ValueType = struct
  type t = value

  let string_of_value = function
    | VInt i -> string_of_int i
    | VBool _ -> "BOOL"
    | VObj _ -> "OBJ"
    | VArray _ -> "ARRAY"
    | Null -> "NULL"
end

module Env = Stack_env.MakeEnv (ValueType)

let rec typ_of_value = function
  | VInt _ -> TInt
  | VBool _ -> TBool
  | VObj obj -> TClass (obj.cls, [])
  | VArray v -> TArray (typ_of_value v.(0))
  | Null -> TVoid

let hashtable_values hashtable =
  Hashtbl.fold (fun _ value acc -> value :: acc) hashtable []

let rec init_value = function
  | TInt -> VInt 0
  | TBool -> VBool false
  | TClass _ -> Null
  | TVoid -> Null
  | TArray t -> VArray (Array.make 0 (init_value t))

  let rec create_array dims t =
    match dims with
    | [] -> failwith "Dimensions list cannot be empty"
    | [dim] -> (
        match t with
        | TInt -> VArray (Array.init dim (fun _ -> VInt 0))
        | TBool -> VArray (Array.init dim (fun _ -> VBool false))
        | TVoid -> VArray (Array.init dim (fun _ -> Null))
        | TClass _ -> VArray (Array.init dim (fun _ -> Null))
        | TArray t ->
            let core_type = Typechecker.elem_type t in
            VArray (Array.init dim (fun _ -> init_value core_type)))
    | dim :: rest -> 
        VArray (Array.init dim (fun _ -> create_array rest t))
  

let report_bug (e : expr) = Tools.report_bug e.loc (fst e.loc).pos_fname

(* let rec get_elem_from_indices (e : expr) value indexes =
  match indexes with
  | [] -> Typechecker.error ("Dimension mismatch" ^ report_bug e)
  | hd :: tl -> (
      let elem = value.(hd) in
      match elem with
      | VArray a -> get_elem_from_indices e a tl
      | _ ->
          if tl = [] then elem
          else Typechecker.error ("Dimension mismatch" ^ report_bug e)) *)

let rec get_elem_from_indices (e : expr) value indexes : (value, error) Result.t
    =
  match indexes with
  | [] -> Error (DimensionMismatch e)
  | hd :: tl -> (
      let elem = value.(hd) in
      match elem with
      | VArray a -> get_elem_from_indices e a tl
      | _ when tl = [] -> Ok elem
      | _ -> Error (DimensionMismatch e))

(** RAISE NotIndexable si type nonindexable*)
let array_of_value v =
  match v with VArray a -> a | _ -> raise (IError (NotIndexable v))

(* main attraction *)
let exec_prog (p : program) : unit =
  let find_class_def class_name = find_class_def p class_name in
  let find_interface_def interface_name =
    find_interface_def p interface_name
  in
  let get_interfaces_from_class class_name =
    let c = find_class_def class_name in
    List.fold_left
      (fun acc name -> find_interface_def name :: acc)
      [] c.implemented_interfaces
  in
  let findclass class_name =
    List.find (fun x -> x.class_name = class_name) p.classes
  in
  let alloc class_name =
    let c = findclass class_name in
    let vartable =
      List.map (fun (name, _, _) -> (name, Null)) c.attributes
      |> List.to_seq |> Hashtbl.of_seq
    in
    { cls = class_name; fields = vartable }
  in

  let rec eval_call f this args =
    let defclass = List.find (fun cls -> cls.class_name = this.cls) p.classes in
    let rec findmethod (defclass : class_def) =
      let definterfaces = get_interfaces_from_class defclass.class_name in
      match
        List.find_opt
          (fun m -> m.method_name = f)
          (defclass.methods
          @ List.filter
              (fun x -> x.default = true)
              (List.flatten
                 (List.map (fun inter -> inter.methods) definterfaces)))
      with
      | Some m -> m
      | None -> (
          match defclass.parent with
          | Some parent ->
              List.find (fun cls -> cls.class_name = parent) p.classes
              |> findmethod
          | None ->
              iraise NotFound
              (* ("Method " ^ f ^ " not found in " ^ defclass.class_name) *)
              (* todo *))
    in
    let method_def = findmethod defclass in
    let method_stack = Env.new_env_stack () in
    let method_env = Env.new_env method_stack in
    (* ajoutarg et this dans env *)
    Env.define_locally method_env "this" (VObj this);
    List.iter2
      (fun (param_name, _) arg -> Env.define_locally method_env param_name arg)
      method_def.params args;
    try
      exec_seq method_def.code method_env;
      Null
    with Return v -> v
  and exec_seq s env_stack =
    let rec evali (e : expr) env_stack =
      match eval e env_stack with VInt n -> n | _ -> assert false
    and evalb (e : expr) env_stack =
      match eval e env_stack with VBool b -> b | _ -> assert false
    and evalo (e : expr) env_stack =
      match eval e env_stack with VObj o -> o | _ -> assert false
    and evalunop unop (e : expr) env_stack =
      let (<:) a b = check_subtype e.loc p b a in
      let (<:?) a b = if not (a <: b) then SubTypeError(a, b) |> tpraise in
      match unop with
      | Opp -> VInt (-evali e env_stack)
      | Not -> VBool (not (evalb e env_stack))
      | TypeCast newType -> (
          let v_e = eval e env_stack in
          (* try *)
            newType <:? (typ_of_value v_e);
            v_e
          (* with _ ->  failwith "todo" *)
          )
          (* Typechecker.TypeError s -> *)
            (* let f = (fst e.loc).pos_fname in
            Typechecker.error (s ^ Tools.report_bug e.loc f)) *)
      | InstanceOf t -> (
          let v_e = eval e env_stack in
          try
            t <:?(typ_of_value v_e);
            VBool true
          with _ -> VBool false)
    and evalbinop binop (e1 : expr) (e2 : expr) env_stack =
      let int_op f = VInt (f (evali e1 env_stack) (evali e2 env_stack)) in
      let bool_op f = VBool (f (evalb e1 env_stack) (evalb e2 env_stack)) in
      let int_to_bool_op f =
        VBool (f (evali e1 env_stack) (evali e2 env_stack))
      in
      let rec ( === ) v1 v2 =
        match (v1, v2) with
        | VInt a, VInt b -> a = b
        | VBool a, VBool b -> a = b
        | Null, Null -> true
        | VObj o1, VObj o2 ->
            assert (o1.cls = o2.cls);
            List.for_all2 ( === )
              (hashtable_values o1.fields)
              (hashtable_values o2.fields)
        | VArray a1, VArray a2 -> Array.for_all2 ( === ) a1 a2
        | _, _ -> Anomaly |> iraise (* should have been typechecked*)
      in
      let ( =/= ) v1 v2 = not (v1 === v2) in
      match binop with
      | Add -> int_op ( + )
      | Sub -> int_op ( - )
      | Mul -> int_op ( * )
      | Div ->
          let e2_val = evali e2 env_stack in
          if e2_val <> 0 then VInt (evali e1 env_stack / e2_val)
          else Division_by_zero e2 |> iraise
      | Rem -> int_op ( mod )
      | Lt -> int_to_bool_op ( < )
      | Le -> int_to_bool_op ( <= )
      | Gt -> int_to_bool_op ( > )
      | Ge -> int_to_bool_op ( >= )
      | Eq -> int_to_bool_op ( = )
      | Neq -> int_to_bool_op ( <> )
      | And -> bool_op ( && )
      | Or -> bool_op ( || )
      | StructEq -> VBool (eval e1 env_stack === eval e2 env_stack)
      | NegStructEq -> VBool (eval e1 env_stack =/= eval e2 env_stack)
    and eval (e : expr) env_stack : value =
      (* print_endline ("evaluation de " ^ string_of_expr e); *)
      match e.expr with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Unop (u, e) -> evalunop u e env_stack
      | Binop (u, e1, e2) -> evalbinop u e1 e2 env_stack
      | Get (Var name) -> Env.find env_stack name
      | Get (Field (obj, field_name)) ->
          let o = evalo obj env_stack in
          Hashtbl.find o.fields field_name
      | Get (Array_var (name, index)) -> (
          (*todo haha :') (c'est moche)*)
          let memtovar = { annot = e.annot; expr = Get name; loc = e.loc } in
          let v = eval memtovar env_stack in
          let int_idxs = List.map (fun x -> evali x env_stack) index in
          let arr = array_of_value v in
          match get_elem_from_indices e arr int_idxs with
          | Ok e -> e
          | Error e -> iraise e)
      | This -> Env.find env_stack "this"
      | New (class_name, g) -> VObj (alloc class_name)
      | NewCstr (class_name, gene, args) ->
          let instance = alloc class_name in
          let n =
            eval_call "constructor" instance
              (List.map (fun x -> eval x env_stack) args)
          in
          if n <> Null then
            (* already checked by compiler *)
            iraise Anomaly (* A constructor must not return anything*)
          else VObj instance
      | MethCall (obj, meth_name, args) ->
          let evaled_args = List.map (fun x -> eval x env_stack) args in
          eval_call meth_name (evalo obj env_stack) evaled_args
      | SuperCall (meth_name, args) ->
          let vobj = Env.find env_stack "this" in
          let obj = match vobj with VObj o -> o | _ -> assert false in
          let parent = (findclass obj.cls).parent in
          let parent_name =
            match parent with Some p -> p | None -> assert false
          in
          let typecasted_obj = { cls = parent_name; fields = obj.fields } in
          eval_call meth_name typecasted_obj
            (List.map (fun x -> eval x env_stack) args)
      | NewArray (t, n) ->
          let idxs = List.map (fun x -> eval x env_stack) n in
          let idx_of_value x =
            match x with
            | VInt n when n > 0 -> n
            | VInt n -> InvalidIndex (e, x) |> iraise (* doit etre > 0*)
            | _ -> InvalidIndex (e, x) |> iraise (* doit etre un int *)
          in
          create_array (List.map idx_of_value idxs) t
      (* | _ -> failwith "case not implemented in eval" *)
    in

    let rec exec (i : instr) env_stack : unit =
      (* print_endline ("execution de " ^ (string_of_instr i)); *)
      match i.instr with
      | Print e -> Printf.printf "%d\n" (evali e env_stack)
      | If (cond, ifseq, elseseq) ->
          exec_seq (if evalb cond env_stack then ifseq else elseseq) env_stack
      | While (cond, iseq) ->
          if evalb cond env_stack then (
            exec_seq iseq env_stack;
            exec { instr = While (cond, iseq); loc = i.loc } env_stack)
      | Set (m, e) -> (
          match m with
          | Var name -> Env.replace env_stack name (eval e env_stack)
          | Field (obj, field_name) ->
              let o = evalo obj env_stack in
              Hashtbl.replace o.fields field_name (eval e env_stack)
          | Array_var (name, i) ->
              (* let v = Env.find env_stack name in *)
              let v =
                eval { annot = e.annot; expr = Get name; loc = e.loc } env_stack
              in
              (*todo v2 hahahaha :')*)
              let evaled_index = List.map (fun x -> evali x env_stack) i in
              let arr = array_of_value v in
              (* todo pouvoir set des array ? *)
              let rec aux a indexes =
                match indexes with
                | [] -> DimensionMismatch e |> iraise
                | hd :: [] -> (
                              
                              match a.(hd) with
                              | VArray a -> DimensionMismatch e |> iraise (*pas assez d'indices*)
                              | _ -> a.(hd) <- eval e env_stack)
                | hd :: tl -> (
                    (* let subarr = array_of_value elem in *)
                    match a.(hd) with
                    | VArray a -> aux a tl
                    | _ -> DimensionMismatch e |> iraise
                    )
              in
              aux arr evaled_index)
      | Return e -> raise (Return (eval e env_stack))
      | Expr e -> ignore (eval e env_stack)
      | Scope s ->
          (* exec seq crée deja une nouvelle portée *)
          exec_seq s env_stack
          (* la portée est dropée touteseule*)
      | Declare (s, _, None) ->
          let f x =
            let declared_locally = Env.is_declared_locally env_stack x in
            if not declared_locally then Env.define_locally env_stack x Null
          in
          List.iter f s
      | Declare (s, _, Some v) ->
          let f x =
            let declared_locally = Env.is_declared_locally env_stack x in
            let rvalue = eval v env_stack in
            let action =
              if declared_locally then Env.replace_locally
              else Env.define_locally
            in
            action env_stack x rvalue
          in
          List.iter f s
    and exec_seq s env_stack =
      (* nouvelle portée *)
      let env_stack = Env.new_env env_stack in
      List.iter (fun instr -> exec instr env_stack) s;
      let _ = Env.pop_local_env env_stack in
      ()
    in
    exec_seq s env_stack
  in
  List.iter (fun (x, _) -> Env.define_globally x Null) p.globals;
  (* p.globals sont toujours le dernier env d'une stack *)
  let main_env_stack = Env.new_env_stack () in
  ignore (exec_seq p.main main_env_stack)
