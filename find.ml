open Kawa

type finderr =
  | ClassNotFound of string * string list
  | InterfaceNotFound of string * string list
  | MethodNotFound of string * string list
  | AttributNotFoud of string * class_def

exception FError of finderr

let fraise x = raise (FError x)

(** @raise ClassNotFound *)
let find_class_def p class_name =
  let classes = p.classes in
  match List.find_opt (fun cls -> cls.class_name = class_name) classes with
  | None ->
      let classes_names = List.map (fun x -> x.class_name) classes in
      ClassNotFound (class_name, classes_names) |> fraise
  | Some c -> c

let find_parent_class_def p sonclassdef =
  match sonclassdef.parent with
  | Some parentname -> Some (find_class_def p parentname)
  | None -> None
(* failwith "devlopper bete ?" todo *)

(** la classe donnée n'est pas incluse *)
let rec get_familly p classdef =
  match classdef.parent with
  | None -> []
  | Some pname ->
      let pardef = find_class_def p pname in
      pardef :: get_familly p pardef

let find_interface_def p interf_name =
  let interfaces = p.interfaces in
  match List.find_opt (fun i -> i.interface_name = interf_name) interfaces with
  | None ->
      let interfaces_names = List.map (fun x -> x.interface_name) interfaces in
      InterfaceNotFound (interf_name, interfaces_names) |> fraise
  | Some i -> i
(* InterfaceNotFound interface_name |> tpraise *)

let get_interfaces p classdef =
  List.map (fun x -> find_interface_def p x) classdef.implemented_interfaces

let signature m = (m.method_name, m.params)

let rec get_all_class_methods p class_def =
  let parents_methods =
    match find_parent_class_def p class_def with
    | Some parentdef -> get_all_class_methods p parentdef
    | None -> []
  in
  let interfaces = get_interfaces p class_def in
  let methodsinterfaces =
    List.flatten (List.map (fun x -> x.methods) interfaces)
  in
  let methodsinterfaces = List.filter (fun x -> x.default) methodsinterfaces in
  let all_local_meths = class_def.methods @ methodsinterfaces in
  let all_local_signatures = List.map signature all_local_meths in
  let filtered_parents_methods =
    List.filter
      (fun m -> not (List.mem (signature m) all_local_signatures))
      parents_methods
  in
  all_local_meths @ filtered_parents_methods

let rec find_method_locally_def p defclass meth_name =
  let meths = get_all_class_methods p defclass in
  match List.find_opt (fun x -> x.method_name = meth_name) meths with
  | None ->
      let methsnames = List.map (fun x -> x.method_name) meths in
      MethodNotFound (meth_name, methsnames) |> fraise
  | Some m -> m

(* error ("Method not found: " ^ meth_name ^ (match closest_string meth_name (List.map (fun m -> m.method_name) allmeth) with
       | Some closest -> ", did you mean " ^ closest ^ " ?\n"
       | None -> "")) *)

(** prog objective curr a <: b <=> a extends b

    si a <: b alors a peut etre utiliser à la place de b *)
let rec check_subtype p soustypede trucquiextends =
  let rec is_primitive_equal t1 t2 =
    match (t1, t2) with
    | TVoid, TVoid -> true
    | TInt, TInt -> true
    | TBool, TBool -> true
    | _ -> false
  in

  match (soustypede, trucquiextends) with
  | TClass (soustypede, _), TClass (trucquiextends, _) ->
      if soustypede = trucquiextends then true
      else
        let soustypede_def = find_class_def p soustypede in
        let trucquiextends_def = find_class_def p trucquiextends in
        let familly = get_familly p trucquiextends_def in
        let verite = List.find_opt (fun x -> x = soustypede_def) familly in
        Option.is_some verite
  | TArray t1, TArray t2 -> check_subtype p t1 t2
  | t1, t2 -> is_primitive_equal t1 t2

(* let find_method_def_locally (classdef : class_def) meth_name =
  let methods = classdef.methods in
  match List.find_opt (fun m -> m.method_name = meth_name) methods with
  | None ->
      let meths_names = List.map (fun x -> x.method_name) methods in
      MethodNotFound (meth_name, meths_names) |> fraise
  | Some m -> m *)

(* raise (NotFound ("Method not found: " ^ meth_name ^ (match closest_string meth_name (List.map (fun m -> m.method_name) methods) with
       | Some closest -> ", did you mean " ^ closest ^ " ?\n"
       | None -> ""))) *)

let create_generic_table (generic_ident : string list) (generic_type : typ list)
    =
  let n = List.length generic_ident in
  assert (n = List.length generic_type);
  let res = Hashtbl.create n in
  List.fold_left2
    (fun table key value ->
      Hashtbl.add table key value;
      table)
    res generic_ident generic_type

(** Sachant une class_def et une list d'application de type generique renvoit le
    vrai type de vartype (qui peut etre Cons<T,U> par ex et renvoit Cons<int,
    bool>)*)
let rec realtypeofgeneric classdef genericstypes vartype =
  let gen_table = create_generic_table classdef.generics genericstypes in
  let res =
    match vartype with
    | TClass (pramnametype, _) ->
        Hashtbl.find_opt gen_table pramnametype |> Option.value ~default:vartype
    | _ -> vartype (* primitive type*)
  in
  let res =
    match res with
    | TClass (n, g) ->
        let fmap x =
          if not (is_primitive x) then
            realtypeofgeneric classdef genericstypes x
          else x
        in
        let gens = List.map fmap g in
        TClass (n, gens)
    | _ -> res
  in
  res

let find_attribut_locally classdef attrname =
  match
    List.find_opt (fun (k, _, _, _) -> k = attrname) classdef.attributes
  with
  | None -> AttributNotFoud (attrname, classdef) |> fraise
  | Some a -> a
