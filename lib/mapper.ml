open Batteries
open Parser
open Yojson

(* Resolve an ID to a given assoc list *)
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


(* Load a configuration given as text *)
exception Invalid_config_mappings
exception Invalid_config_imports
let rec loadConfiguration (config : string) (dir : string) : (string -> string) list =
  let imports =
    let json = Yojson.Basic.from_string config in
    let files = Yojson.Basic.Util.member "imports" json in
    match files with
    | `Null -> []
    | `List a -> (
        let file_names = List.map (fun data ->
            match data with
            | `String f -> f
            | _ -> raise Invalid_config_imports
          ) a
        in
        List.fold_left (fun ret f ->
            let f_name = Filename.concat dir f in
            let file = File.open_in f_name in
            let config_text = IO.read_all file in
            (loadConfiguration config_text (Filename.dirname f_name))@ret
          ) [] file_names
      )
    | _ -> raise Invalid_config_imports
  in
  let inline_mappings =
    let json = Yojson.Basic.from_string config in
    let inline = Yojson.Basic.Util.member "mappings" json in
    match inline with
    | `Null -> []
    | `Assoc a -> (resolve  a)::[]
    | _ -> raise Invalid_config_mappings
  in
  inline_mappings@imports


(* Collect all mappings into a list of mappings *)
exception Not_config
let collectMappings segments dir =
  let configs = segments |> List.filter (fun seg ->
      match seg with
      | Config(_) -> true
      | _ -> false
    )
  in
  List.fold_left (fun maplist c ->
      match c with
      | Config(x) -> (loadConfiguration x dir)@maplist
      | _ -> raise Not_config
    ) [] configs


(* Apply mappings to an id *)
exception No_mapping of string
let rec applyMap maps id =
  match maps with
  | [] -> raise (No_mapping id)
  | h::t -> try
      h id
    with
    | _ -> applyMap t id


(* Replace the variables in the segment list using mapping *)
exception Missing_mapping of string
let replace mapping segments =
  List.map (fun n ->
      match n with
      | Text(x) -> Text(x)
      | Config(x) -> Text("")
      | Variable(x) ->
        try
          Text(mapping x)
        with
        | _ -> raise (Missing_mapping(x))
    ) segments
