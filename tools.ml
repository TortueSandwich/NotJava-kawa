open Lexing
open Printf

let min3 a b c = min a (min b c)

let levenshtein_distance s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let matrix = Array.make_matrix (len1 + 1) (len2 + 1) 0 in

  for i = 0 to len1 do
    matrix.(i).(0) <- i
  done;

  for j = 0 to len2 do
    matrix.(0).(j) <- j
  done;

  for i = 1 to len1 do
    for j = 1 to len2 do
      let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
      matrix.(i).(j) <- min3
        (matrix.(i - 1).(j) + 1)    (* deletion *)
        (matrix.(i).(j - 1) + 1)    (* insertion *)
        (matrix.(i - 1).(j - 1) + cost) (* substitution *)
    done;
  done;

  matrix.(len1).(len2)

let closest_string target lst =
  let rec find_closest closest_str closest_dist = function
    | [] -> Some (closest_str)
    | hd :: tl ->
        let dist = levenshtein_distance target hd in
        if dist < closest_dist then
          find_closest hd dist tl
        else
          find_closest closest_str closest_dist tl
  in
  match lst with
  | [] -> None
  | hd :: tl -> find_closest hd (levenshtein_distance target hd) tl

let get_string_from_file filename (start_pos, end_pos) =
  let ic = open_in filename in
  let buffer = Buffer.create (end_pos.pos_cnum - start_pos.pos_cnum) in
  try
    seek_in ic start_pos.pos_cnum;
    for _ = start_pos.pos_cnum to end_pos.pos_cnum - 1 do
      Buffer.add_char buffer (input_char ic)
    done;
    close_in ic;
    Buffer.contents buffer
  with e ->
    close_in_noerr ic;
    raise e

let get_line filename line_num =
  let ic = open_in filename in
  let rec loop n =
    if n = 0 then input_line ic
    else (ignore (input_line ic); loop (n - 1))
  in
  let result = loop (line_num - 1) in
  close_in ic;
  result

let report_bug (pos: Lexing.position * Lexing.position) (f: string) =
  let b, e = pos in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  let line_content = get_line f l in
  let error_indicator = String.make (fc - 1) ' ' ^ String.make (lc - fc + 1) '^' in
  sprintf "\nFile \"%s\", line %d, characters %d-%d:\n%s\n%s" f l fc lc line_content error_indicator

let count_bracket_pairs (s: string) : int =
  let len = String.length s in
  let rec aux i count =
    if i >= len - 1 then count
    else if s.[i] = '[' && s.[i + 1] = ']' then aux (i + 2) (count + 1)
    else aux (i + 1) count
  in
  aux 0 0

let is_valid_array_string (s: string) : bool =
  let len = String.length s in
  let rec aux i =
    if i >= len then true
    else if s.[i] = '[' && i + 1 < len && s.[i + 1] = ']' then aux (i + 2)
    else if s.[i] = '[' || s.[i] = ']' then false
    else aux (i + 1)
  in
  aux 0



