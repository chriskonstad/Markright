(* TODO Be able to take in config as a file or a string *)
(* TODO Add commandline flags *)
(* TODO Write to output, take input somehow *)
(* TODO Modularize and add build system/files *)
(* TODO Test how trimming affects parsing *)
(* TODO Change from reading in how file to reading line by line at some point *)

(*#require "batteries"*)
(*#require "yojson"*)
open Core.Std
open Batteries
open Yojson

type segment =
  | Text of string
  | Variable of string

let var_start_string = "{{"
let var_end_string = "}}"

(* Parse text into a segment list *)
let rec parse text =
  try
    let var_start = Batteries.String.find text var_start_string in
    let var_end = String.find text var_end_string in
    if var_start != 0 && text.[var_start-1] = '\\' then (
      (* Escaped *)
      let next_ind = String.find_from text (var_start + 1) var_start_string in
      Text(String.sub text 0 next_ind) ::
      parse (String.sub text next_ind ((String.length text) - next_ind))
    ) else
      let var_length = var_end - var_start + (String.length var_end_string) in
      Text(String.sub text 0 var_start) ::
      Variable(let name = String.sub text var_start var_length in
               let name_length = (String.length name) -
                                 (String.length var_start_string) -
                                 (String.length var_end_string) in
               String.trim (String.sub name
                              (String.length var_start_string)
                              name_length)
              ) ::
      parse (String.sub text (var_start + var_length) ((String.length text) -
                                                       (var_start + var_length)))
  with
  (* No variables found *)
  | _ -> Text(text)::[]


(* Remove empty text segments *)
let filter_empty_text segments =
  List.filter (fun s ->
      match s with
      | Text(t) -> if t = "" then false else true
      | _ -> true
    ) segments


(* Replace the variables in the segment list using mapping *)
exception Missing_mapping
let replace mapping segments =
  try
    List.map (fun n ->
        match n with
        | Text(x) -> Text(x)
        | Variable(x) -> Text(mapping x)
      ) segments
  with
  | _ -> raise Missing_mapping


exception Not_text of string
(* Flatten the segment list into the resulting string *)
let flatten segments =
  let strings = List.map (fun n ->
      match n with
      | Text(x) -> x
      | Variable(x) -> raise (Not_text x)
    ) segments
  in
  String.concat "" strings


(********************* JSON variable configuration loading ********************)
(* With a given json assoc array and id, return the json's value for id *)
exception Invalid_config_data
exception Invalid_config_id
let resolve json id =
  let rec resolver ids json =
      match ids with
          | [] -> raise Invalid_config_id
          | h::t -> let value = List.assoc h json in
            match value with
              | `Assoc a -> resolver t a
              | `String s -> s
              (* TODO Handle floats, ints, etc? *)
              | _ -> raise Invalid_config_data
  in
  resolver (String.nsplit id ".") json


(* Load json as the variable mapping function *)
let loadMap json =
  resolve (Yojson.Basic.Util.to_assoc json)


(* Load json from a file *)
let loadMapFile file =
  let json  = Yojson.Basic.from_file file in
  loadMap json


let markright mapping text =
  parse text |> filter_empty_text |> replace mapping |> flatten


(* Use the given config file to run markright on the text *)
let markright_with_config config text =
  markright (loadMapFile config) text


let load_file (f : string) : string =
  let file = File.open_in f in
  IO.read_all file


let process_file (config : string) (file : string) : string =
  let text = load_file file in
  markright_with_config config text

let command =
  Command.basic
    ~summary:"Convert a Markright file to Markdown"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
                  +> anon ("config_file" %: file)
                  +> anon ("markright_file" %: file)
                 )
    (fun config text () -> process_file config text |> print_string)


let () =
  Command.run ~version:"1.0" command
