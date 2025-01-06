{
  open Lexing
  open Kawaparser

  exception Error of string

  let last_lexbuf= ref ""

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "print",    PRINT;
        "main",     MAIN;
        "var",      VAR;
        "if",       IF;
        "else",     ELSE;
        "while",    WHILE;
        "interface" , INTERFACE;
        "class",    CLASS;
        "extends",  EXTENDS;
        "implements",  IMPLEMENTS;
        "attribute",ATTRIBUTE;
        "method",   METHOD;
        "default" , DEFAULT;
        "new",      NEW;
        "true",     BOOL(true);
        "false",    BOOL(false);
        "this",     THIS;
        "return",   RETURN;
        "int",      TINT;
        "bool",     TBOOL;
        "void",     TVOID;
        "as",       AS;
        "instanceof",  INSTANCEOF;
        "super" ,   SUPER;
        (* "impl",     IMPL; *)
        "generic",   GENERIC;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

  let violet = "\027[35m"
  let jaune = "\027[33m"
  let vert = "\027[92m"
  let orange = "\027[38:5:208m"
  let bleu = "\027[94m"

  let token_to_string s = 
    let t = match s with
    | PRINT -> violet^"print"
    | MAIN -> "\027[34mmain"
    | VAR -> violet^"var "
    | IDENT(s) ->  "\027[36mId("^s^")"
    | INT(n) -> "\027[96m" ^ (string_of_int n)
    | BOOL(b:bool) -> "\027[96m" ^ (string_of_bool b)
    | SEMI -> jaune^";"
    | LPAR -> jaune^"("
    | RPAR -> jaune^")"
    | BEGIN -> jaune^"{"
    | END -> jaune^"}"
    | COMA -> jaune^","
    | PLUS -> vert^"+"
    | MINUS -> vert^"-"
    | TIMES -> vert^"*"
    | DIV -> vert^"/"
    | EQ -> vert^"=="
    | NEQ -> vert^"!="
    | EXCLAMATION -> vert^"!"
    | MOD -> vert^"%"
    | AFFECT -> violet^"="
    | POINT -> "\027[4m."

    | LT -> violet^"<"
    | LEQ -> violet^"<="
    | GT -> violet^">"
    | GEQ -> violet^">="
    | AND -> violet^"&&"
    | OR -> violet^"||"

    | EOF -> "EOF"
    | WHILE -> violet^"while"
    | IF -> violet^"if"
    | ELSE -> violet^"else"
    | CLASS -> violet^"class"
    | INTERFACE -> violet^"interface"
    | EXTENDS -> violet^"extends"
    | IMPLEMENTS -> violet^"implements"
    | RETURN -> violet^"return"
    | NEW -> violet^"new"
    | ATTRIBUTE -> violet^"attribute"
    | METHOD -> violet^"method"
    | AS -> violet^"as"
    | THIS -> orange ^"this"
    | INSTANCEOF -> violet^"instanceof"
    | GENERIC -> bleu^"generic"
    | DEFAULT -> violet^"default"
    (* | IMPL -> violet^"impl" *)

    | TINT -> bleu^"int"
    | TBOOL -> bleu^"bool"
    | TVOID -> bleu^"void"

    | RBR -> jaune^"]"
    | LBR -> jaune^"["
    | SUPER -> orange^"super"

    (* | _ -> "UNKNOWN"  *)
    in t ^"\027[0m"    

  let type_context = ref false

  let is_type_context () = !type_context
  let set_type_context b = type_context := b
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*

rule token = parse
  | ['\n']            {  new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  {  token lexbuf }

  | "//" [^ '\n']* "\n"  {  new_line lexbuf; token lexbuf }
  | "/*"                 {  comment lexbuf; token lexbuf }

  | number as n  {  INT(int_of_string n) }
  | ident as id  {  keyword_or_ident id }

  | ";"  {  SEMI }
  | "("  {  LPAR }
  | ")"  {  RPAR }
  | "{"  {  BEGIN }
  | "}"  {  END }

  | "+"  {  PLUS }
  | "-"  {  MINUS }
  | "*"  {  TIMES }
  | "/"  {  DIV }
  | "%"  {  MOD }
  | "==" {  EQ }
  | "=" {  AFFECT }
  | "!=" {  NEQ }

  | "<" { LT }
  | "<=" {  LEQ }
  | ">" { GT } 
  | ">=" {  GEQ }
  | "&&" {  AND }
  | "||" {  OR }
  | "!" {  EXCLAMATION }

  | '.' {  POINT }
  | ',' {  COMA }

  | "instanceof" {  INSTANCEOF }

  | '[' {  LBR }
  | ']' {  RBR }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { let tok = EOF in tok }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }