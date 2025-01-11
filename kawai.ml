(* Point d'entrée / Main*)
(* modifications :
   std.Arg pour les paramètres de l'.exe
   possibilitée de selectioner .kwa si non donné à l'.exe
   possibilitée d'afficher le code source du .kwa
   possibilitée d'interpreter plusieur fichier en une fois
   afficher commande pour compilier fichier choisi si non-precisé à l'.exe
   couleurs :)
*)

open Format
open Lexing

open Arg

(* lis les parametres donnée à l'exe*)
let usage_msg = "kawai <file1> [--show-source]"
let show_source = ref false
let input_files = ref []
let generate_dot = ref false

let speclist =
  [
    ("--show_source", Arg.Set show_source, "Print read file");
    ("-s", Arg.Set show_source, "Print read file");
    ("-d", Arg.Set generate_dot, "Generate a graphviz visualisation of the AST");
  ]

let anon_fun filename = input_files := filename :: !input_files
let () = Arg.parse speclist anon_fun usage_msg

(* trouver tout les .kwa en partant de dir *)
let rec get_files_with_extension ext dir =
  let files = Sys.readdir dir in
  let rec process_files = function
    | [] -> []
    | file :: rest ->
        let full_path = Filename.concat dir file in
        if file = "_build" then process_files rest
        else if Sys.is_directory full_path then
          get_files_with_extension ext full_path @ process_files rest
        else if Filename.check_suffix file ext then
          full_path :: process_files rest
        else process_files rest
  in
  process_files (Array.to_list files)

(* Afficher code source *)
let print_source file =
  let in_channel = open_in file in
  try
    while true do
      let line = input_line in_channel in
      print_endline line
    done
  with End_of_file -> close_in in_channel

(* recuperer fichiers si non-précisé *)
let files =
  match !input_files with
  | [] ->
      let kwa_files = get_files_with_extension ".kwa" "." in
      if kwa_files = [] then (
        print_endline "Erreur : Aucun fichier .kwa trouvé à partir d'ici";
        exit 1)
      else (
        print_endline "\027[93mAucun fichier source spécifié.\027[0m";
        print_endline "Voici les fichiers .kwa disponibles :";
        List.iteri (fun i file -> Printf.printf "  [%d] %s\n" i file) kwa_files;
        Printf.printf
          "Veuillez entrer le numéro du fichier à utiliser :\n\027[92m>\027[0m ";
        let choice = read_line () in
        match int_of_string_opt choice with
        | Some index when index >= 0 && index < List.length kwa_files ->
            [ List.nth kwa_files index ]
        | _ ->
            print_endline "\027[91mChoix invalide.\027[0m";
            (* faut faire des efforts :|*)
            exit 1)
  (* todo corriger chemain de fichier incomplet (manque .kwa ou ./tests/)*)
  | filename -> filename

let load_file_content filename =
  let ic = open_in filename in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  read_lines []

let get_line filename line_num =
  let lines = load_file_content filename in
  try List.nth lines (line_num - 1) with _ -> ""

(* affichage les tokens d'un lexer *)
let lex_and_print_tokens c =
  let lb = Lexing.from_channel c in
  let rec loop ligne indent =
    let tok = Kawalexer.token lb in
    let pos = lb.lex_curr_p.pos_lnum in
    let new_indent =
      match tok with
      | BEGIN -> indent + 2
      | END -> max 0 (indent - 2)
      | _ -> indent
    in
    if pos <> ligne then Printf.printf "\n%s" (String.make new_indent ' ');
    Printf.printf "%s" (Kawalexer.token_to_string tok);
    if tok <> EOF then loop pos new_indent
  in
  try
    loop 1 0;
    print_endline ""
  with
  | End_of_file -> ()

let extract_parentheses_content s =
    try
      let start_idx = String.index s '(' in
      let end_idx = String.index s ')' in
      if start_idx < end_idx then
        let content = String.sub s (start_idx + 1) (end_idx - start_idx - 1) in
        let content = 
          if String.length content >= 2 && content.[0] = '"' && content.[String.length content - 1] = '"' then
            String.sub content 1 (String.length content - 2)
          else content
        in
        content
      else
        raise (Invalid_argument "Invalid string format")
    with Not_found -> raise (Invalid_argument "Parentheses not found")
  
let custom_printexc_to_string exn =
    let original_output = Printexc.to_string exn in
    let extracted_content = 
      try extract_parentheses_content original_output 
      with Invalid_argument _ -> original_output
    in
    "\027[91m SyntaxError:\027[0m " ^ extracted_content

let () =
  let exit code =
    if !input_files = [] then
      eprintf "\027[2mRe-execute using\n./kawai.exe %s %s\027[0m\n"
        (List.nth files 0)
        (if !show_source then "-s" else "");
    exit code
  in
  let compile f =
    let report loc = print_string (Tools.report_bug loc f)  in
    let c = open_in f in
    let lb = Lexing.from_channel c in
    Lexing.set_filename lb f;
    if !show_source then (
      Printf.printf "\027[2mSource code of %s :\027[0m\n" f;
      lex_and_print_tokens (open_in f));
    try
      Printf.printf "\027[2mOutput of %s :\027[0m\n" f;
      flush stdout;
      let prog = Kawaparser.program Kawalexer.token lb in
      close_in c;
      let typed_prog = Typechecker.typecheck_prog prog in
      if !generate_dot then Visuast.main typed_prog;
      Interpreter.exec_prog typed_prog
      (* Interpreter.exec_prog prog *)
    with
    | Kawalexer.Error s ->
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "\027[91mlexical error:\027[0m (lexer) %s@." s;
        eprintf "\027[0m";
        exit 1
    | Kawaparser.Error ->
        (*A modifier pour enlever les éléments liées à menhir*)
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "\027[91msyntax error\027[0m (parser)@.";
        eprintf "\027[91msyntax error:\027[0m Unexpected token: %s\n"
          (Kawalexer.token_to_string (Kawalexer.token lb));
        exit 1
    | Interpreter.Error s ->
        eprintf "\027[91minterpreter error: \027[0m%s@." s;
        exit 1
    | Typechecker.TypeCheckerError (err,loc) -> 
        match err with 
        | Typechecker.TypeError s -> eprintf "\027[91mType error: \027[0m%s@.\n" s; report loc ;
        | Typechecker.IndexOutOfBounds -> eprintf "\027[91mCompile Time error: \027[0m Index out of bounds@.\n" ; report loc ;
        | Typechecker.DimensionMismatch -> eprintf "\027[91mType error: \027[0m Dimension Mismatch@.\n" ; report loc ;
        | Typechecker.TypeCastError s -> eprintf"\027[91mTypeCast error: \027[0m%s@.\n" s; report loc ;
        | Typechecker.NotFound s -> eprintf "\027[91mUnknown Reference: \027[0m%s@.\n" s; report loc ;
        | Typechecker.CompileTimeError s -> eprintf "\027[91mCompileTimeError: \027[0m%s@.\n" s; report loc ;

        exit 1
    | Stack_env.EnvError e ->
      eprintf "\027[91mEnvironment error:\027[0m %s@." (Stack_env.string_of_env_error e);
      exit 1
    | e ->
        eprintf "%s\n@." (custom_printexc_to_string e); 
        report (lexeme_start_p lb, lexeme_end_p lb);
        (* lex_and_print_tokens (open_in f); *)
        exit 2
  in
  List.iter compile (List.rev files);
  exit 0
