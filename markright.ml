(* TODO Text parser *)
(* TODO Variable mapper *)
(* TODO Config loader *)
(* TODO Input/output *)

open Batteries

type segment =
  | Text of string
  | Variable of string

let var_start_string = "{{"
let var_end_string = "}}"

(* Parse text into a segment list *)
(* TODO Don't forget to trim whitespace from variable name *)
let rec parse text =
  try
    let var_start = String.find text var_start_string in
    let var_end = String.find text var_end_string in
    if var_start != 0 && text.[var_start-1] = '\\' then (
      (* Escaped *)
      let next_ind = String.find_from text (var_start + 1) var_start_string in
      Text(String.sub text 0 next_ind) :: parse (String.sub text next_ind ((String.length text) - next_ind))
    ) else
      let var_length = var_end - var_start + (String.length var_end_string) in
      Text(String.sub text 0 var_start) :: Variable(String.sub text var_start var_length) :: parse (String.sub text (var_start + var_length) ((String.length text) - (var_start + var_length)))
  with
  (* No variables found *)
  | _ -> Text(text)::[]


let filter_empty_text segments =
  List.filter (fun s ->
      match s with
      | Text(t) -> if t = "" then false else true
      | _ -> true
    ) segments


(*let var_regex = Str.regexp "\\(^{{[A-Za-z0-9]+}}\\)"*)
(*let var_name_regex = Str.regexp "\\([A-Za-z0-9]+\\)"*)
(* TODO Don't forget to trim whitespace from variable name *)
(*let node_to_segment nodes =*)
(*  List.map (fun n ->*)
(*      try*)
(*        Str.search_forward var_regex n 0;*)
(*        Str.search_forward var_name_regex n 0;*)
(*        [> TODO Make sure able to escape variables! Use nested regex? <]*)
(*        Variable(Str.matched_string n)*)
(*      with*)
(*      | Not_found -> Text(n)*)
(*    ) nodes*)

(* Replace the variables in the AST using mapping *)
let replace nodes mapping =
  List.map (fun n ->
      match n with
      | Text(x) -> Text(x)
      | Variable(x) -> Text(mapping x)
    ) nodes

exception Not_text of string

(* Flatten the AST into the resulting string *)
let flatten nodes =
  let strings = List.map (fun n ->
      match n with
      | Text(x) -> x
      | Variable(x) -> raise (Not_text x)
    ) nodes
  in
  String.concat "" strings

(* Use {{var_name}} to be a variable use *)
