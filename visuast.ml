open Kawa
open Printf

(* Fonction pour générer un identifiant unique *)
let fresh_id =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    incr counter;
    id

let create_node id label typ =
  if typ = "" then Printf.sprintf "  node%d [label=\"%s\"];" id label
  else
    Printf.sprintf
      "  node%d [label=<%s <BR/><FONT POINT-SIZE=\"8.0\">%s</FONT>>];" id label
      typ

let create_connection ?(label = "") from_id to_id =
  if label = "" then Printf.sprintf "  node%d -> node%d;" from_id to_id
  else Printf.sprintf "  node%d -> node%d [label=\"%s\"];" from_id to_id label

let rec expr_to_dot with_id (e : expr) =
  let t = e.annot in
  let ts = string_of_typ t in
  let create_node with_id lab = create_node with_id lab ts in
  match e.expr with
  | Int i -> ([ create_node with_id (string_of_int i) ], [])
  | Bool b -> ([ create_node with_id (string_of_bool b) ], [])
  | Binop (opp, e1, e2) ->
      let op_str = string_of_biop opp in
      let currnode = create_node with_id op_str in
      let argnodes, argcon = create_node_and_connections with_id [ e1; e2 ] in
      (currnode :: argnodes, argcon)
  | Get m ->
      let currnode = create_node with_id "get" in
      let m_id = fresh_id () in
      let m_nodes, m_connections = mem_to_dot m_id m in
      let c = create_connection with_id m_id in
      (currnode :: m_nodes, c :: m_connections)
  | This -> ([ create_node with_id "this" ], [])
  | SuperCall (s, el) -> ([ create_node with_id "super" ], [])
  | New (s, g) ->
      let currnode = create_node with_id "new" in
      let c_id = fresh_id () in
      let cls_node = create_node c_id s in
      let con = create_connection with_id c_id in
      ([ currnode; cls_node ], [ con ])
  | NewCstr (s, gene, l) ->
      let currnode = create_node with_id "newCstr" in
      let c_id = fresh_id () in
      let cls_node =
        create_node c_id
          (s ^ List.fold_left (fun acc x -> acc ^ string_of_typ x) "" gene)
      in
      let con = create_connection with_id c_id in
      let argnodes, argcon =
        create_node_and_connections with_id l ~ordered:true
      in
      ([ currnode; cls_node ] @ argnodes, con :: argcon)
  | MethCall (e, s, l) ->
      let currnode = create_node with_id "." in
      let c_id = fresh_id () in
      let cls_node = create_node c_id s in
      let e_id = fresh_id () in
      let e_node, e_con = typed_expr_to_dot e_id e in
      let con =
        [
          create_connection with_id c_id ~label:"calls";
          create_connection with_id e_id ~label:"on";
        ]
      in
      let argnodes, argcon =
        create_node_and_connections with_id l ~ordered:true
      in
      ([ currnode; cls_node ] @ e_node @ argnodes, con @ e_con @ argnodes)
  | Unop (opp, e) -> (
      let op_str = string_of_unop opp in
      let currnode = create_node with_id op_str in
      let argnodes, argcon = create_node_and_connections with_id [ e ] in
      match opp with
      | TypeCast t ->
          let t_id = fresh_id () in
          let t_node = create_node t_id (string_of_typ t) in
          let t_con = create_connection with_id t_id in
          (currnode :: t_node :: argnodes, t_con :: argcon)
      | _ -> (currnode :: argnodes, argcon))
  | NewArray (t, args) -> ([], [])

(* | _ ->
      let node = create_node with_id "Non traité (expr)" in
      ([ node ], []) *)

and typed_expr_to_dot with_id expr = expr_to_dot with_id expr

and create_node_and_connections ?(ordered = false) with_id
    (childrens : expr list) =
  let l = List.map (fun x -> (fresh_id (), x)) childrens in
  let connections =
    List.mapi
      (fun i (id, _) ->
        create_connection with_id id
          ~label:(if ordered then string_of_int i else ""))
      l
  in
  let l = List.map (fun (id, x) -> typed_expr_to_dot id x) l in
  let n, c = List.split l in
  let concat acc x = x @ acc in
  (List.fold_left concat [] n, connections @ List.fold_left concat [] c)

and mem_to_dot withid (m : Kawa.mem_access) =
  match m with
  | Var s ->
      let currnode = create_node withid s "" in
      ([ currnode ], [])
  | Field (e, s) ->
      let f_id = fresh_id () in
      let e_id = fresh_id () in
      let n, c = typed_expr_to_dot e_id e in
      let nodes = [ create_node f_id s ""; create_node withid "." "" ] in
      let connections =
        [ create_connection withid e_id; create_connection withid f_id ]
      in
      (nodes @ n, connections @ c)
  | Array_var (s, el) ->
      let currnode = create_node withid "Array_var" "" in
      let argnodes, argcon =
        create_node_and_connections withid el ~ordered:true
      in
      let id_var = fresh_id () in
      let varnode = create_node id_var (string_of_mem s) "" in
      let cons = create_connection withid id_var ~label:"array" in
      (currnode :: varnode :: argnodes, cons :: argcon)
(* | _ -> let node = create_node withid "Non traité (memory)" "" in ([ node ], []) *)

and inst_to_dot with_id instr =
  let create_node with_id lab = create_node with_id lab "" in
  match instr with
  | Print e ->
      let currnode = create_node with_id "Print" in
      let e_id = fresh_id () in
      let e_nodes, e_connections = typed_expr_to_dot e_id e in
      let connections = [ create_connection with_id e_id ] in
      (currnode :: e_nodes, connections @ e_connections)
  | Set (mem, e) ->
      let currnode = create_node with_id "=" in
      let e_id = fresh_id () in
      let m_id = fresh_id () in
      let e_nodes, e_connections = typed_expr_to_dot e_id e in
      let m_nodes, m_connections = mem_to_dot m_id mem in
      let connections =
        [
          create_connection with_id e_id ~label:"value";
          create_connection with_id m_id ~label:"set";
        ]
      in
      ( (currnode :: e_nodes) @ m_nodes,
        connections @ e_connections @ m_connections )
  | While (e, s) ->
      let e_id = fresh_id () in
      let m_id = fresh_id () in
      let currnode = create_node with_id "While" in
      let e_nodes, e_connections = typed_expr_to_dot e_id e in
      let s_nodes, s_connections = seq_to_dot s m_id in
      let connections =
        [
          create_connection with_id e_id ~label:"cond";
          create_connection with_id m_id ~label:"instrs";
        ]
      in
      ( (currnode :: e_nodes) @ s_nodes,
        connections @ e_connections @ s_connections )
  | If (e, s1, s2) ->
      let e_id = fresh_id () in
      let s1_id = fresh_id () in
      let s2_id = fresh_id () in
      let currnode = create_node with_id "If" in
      let e_nodes, e_connections = typed_expr_to_dot e_id e in
      let s1_nodes, s1_connections = seq_to_dot s1 s1_id in
      let s2_nodes, s2_connections = seq_to_dot s2 s2_id in
      let connections =
        [
          create_connection with_id e_id ~label:"cond";
          create_connection with_id s1_id ~label:"instrs if";
          create_connection with_id s2_id ~label:"instrs else";
        ]
      in
      ( (currnode :: e_nodes) @ s1_nodes @ s2_nodes,
        connections @ e_connections @ s1_connections @ s2_connections )
  | Return e ->
      let currnode = create_node with_id "Return" in
      let e_id = fresh_id () in
      let enode, e_con = typed_expr_to_dot e_id e in
      ([ currnode ], e_con)
  | Expr e ->
      let currnode = create_node with_id "Expr as instr" in
      let e_id = fresh_id () in
      let connec = create_connection with_id e_id ~label:"expr" in
      let e_node, e_connec = expr_to_dot e_id e in
      (currnode :: e_node, connec :: e_connec)
  | Scope instrs ->
      let currnode = create_node with_id "Scope" in
      let new_id = fresh_id () in
      let s_nodes, s_connections = seq_to_dot instrs new_id in
      let conec = create_connection with_id new_id ~label:"instrs" in
      (currnode :: s_nodes, conec :: s_connections)
  | Declare (vars, t, value) -> ([], [])

(* | _ ->
      let node = create_node with_id "Non traité (instr)" in
      ([ node ], []) *)

and seq_to_dot seq withid =
  let rec aux nodes connections prev_id count = function
    | [] -> (nodes, connections)
    | instr :: tail ->
        let curr_id = fresh_id () in
        let instr_nodes, instr_connections = inst_to_dot curr_id instr in
        let connection =
          create_connection prev_id curr_id ~label:(string_of_int count)
        in
        aux (nodes @ instr_nodes)
          (connections @ instr_connections @ [ connection ])
          curr_id (count + 1) tail
  in
  aux [] [] withid 1 (List.map (fun i -> i.instr) seq)

let program_to_dot program output_file =
  let main_id = fresh_id () in
  let main_node = create_node main_id "main" "" in
  let nodes, connections = seq_to_dot program.main main_id in

  let dot_content =
    "digraph AST {\n"
    ^ String.concat "\n" (main_node :: nodes)
    ^ "\n"
    ^ String.concat "\n" connections
    ^ "\n}"
  in
  let oc = open_out output_file in
  output_string oc dot_content;
  close_out oc

let main (p : program) =
  program_to_dot p "kawa_ast.dot";
  Printf.printf "AST DOT file generated: kawa_ast.dot\n";

  let check_dot = Sys.command "which dot > /dev/null 2>&1" in
  if check_dot <> 0 then
    Printf.printf
      "Error: The 'dot' command is not installed. Please install Graphviz.\n"
  else
    let result = Sys.command "dot -Tpng kawa_ast.dot -o kawa_ast.png" in
    if result <> 0 then
      Printf.printf
        "Error: Failed to execute 'dot' command. Check your DOT file and \
         Graphviz installation.\n\
         (or error in dot, try ``dot -Tpng kawa_ast.dot -o kawa_ast.png`` ?)\n"
    else Printf.printf "Image generated: kawa_ast.png\n"
