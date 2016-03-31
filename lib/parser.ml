(** Parses Markright text into an abstract representation *)
open Batteries


(** Represents the different types of nodes in a Markright file *)
type segment =
  | Text of string
  | Variable of string
  | Config of string


(** The Markright segment opening string *)
let var_start_string = "{{"
(** The Markright segment closing string *)
let var_end_string = "}}"

(** Parse text into a segment list *)
let rec parse text =
  try
    let var_start = Batteries.String.find text var_start_string in
    let var_end = String.find text var_end_string in
    if var_start != 0 && text.[var_start-1] = '\\' then (
      (* Escaped, also consumes escape char *)
      try
        let next_ind = String.find_from text (var_start + 1) var_start_string in
        Text(String.sub text 0 (var_start-1)) ::
        Text(String.sub text var_start (next_ind-var_start)) ::
        parse (String.sub text next_ind ((String.length text) - next_ind))
      with
      | Not_found ->
        Text(String.sub text 0 (var_start-1)) ::
        Text(String.sub text (var_start) ((String.length text) - var_start)) ::
        []
    ) else
      let var_length = var_end - var_start + (String.length var_end_string) in
      let consume_newline =
        try
          if text.[var_end + (String.length var_end_string)] = '\n' then 1 else 0
        with
        (* if character past end of string *)
        | _ -> 0
      in
      Text(String.sub text 0 var_start) ::
      let next_start = var_start + var_length + consume_newline in
      let next = parse (String.sub
                          text
                          next_start
                          ((String.length text) - next_start)) in
      let name = String.sub text var_start var_length in
      let name_length = (String.length name) -
                        (String.length var_start_string) -
                        (String.length var_end_string) in
      let contents = String.trim (String.sub name
                     (String.length var_start_string)
                     name_length) in
      if '{' = contents.[0] then
        Config(contents)::next
      else
        Variable(contents)::next
  with
  (* No variables found *)
  | x -> Text(text)::[]


(** Remove empty text segments *)
let filter_empty_text segments =
  List.filter (fun s ->
      match s with
      | Text(t) -> if t = "" then false else true
      | _ -> true
    ) segments


(** There is a non-Text segment when there shouldn't be *)
exception Not_text of string
(** Flatten the segment list into the resulting string *)
let flatten segments =
  let strings = List.map (fun n ->
      match n with
      | Text(x) -> x
      | Variable(x) -> raise (Not_text x)
      | Config(x) -> raise (Not_text x)
    ) segments
  in
  String.concat "" strings
