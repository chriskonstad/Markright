(* TODO Text parser *)
(* TODO Variable mapper *)
(* TODO Config loader *)
(* TODO Input/output *)

type segment =
  | Text of string
  | Variable of string

let var_regex = Str.regexp "\\(^{{[A-Za-z0-9]+}}\\)"
let var_name_regex = Str.regexp "\\([A-Za-z0-9]+\\)"
let node_to_segment nodes =
  List.map (fun n ->
      try
        Str.search_forward var_regex n 0;
        Str.search_forward var_name_regex n 0;
        (* TODO Make sure able to escape variables! Use nested regex? *)
        Variable(Str.matched_string n)
      with
      | Not_found -> Text(n)
    ) nodes

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
