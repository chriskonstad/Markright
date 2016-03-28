open Batteries
open Parser
open Yojson

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


(* Load a list of json files *)
let loadMapFiles (files : string list) =
  List.fold_left (fun lst file ->
      (loadMapFile file)::lst
    ) [] files


(* Collect all mappings into a list of mappings *)
exception Invalid_config_mappings
exception Invalid_config_imports
let collectMappings segments =
  let configs = segments |> List.filter (fun seg ->
      match seg with
      | Config(_) -> true
      | _ -> false
    )
  in
  let imports =
    List.fold_left (fun map_list segment ->
        match segment with
        | Config(x) ->
          let json = Yojson.Basic.from_string x in
          let files = Yojson.Basic.Util.member "imports" json in (
            match files with
            | `Null -> map_list
            | `List a -> (
                let file_names = List.map (fun data ->
                    match data with
                    | `String f -> f
                    | _ -> raise Invalid_config_imports
                  ) a
                in
                loadMapFiles file_names
              )@map_list
            | _ -> raise Invalid_config_imports
          )
        | _ -> map_list
      ) [] configs
  in
  let inline_mappings =
    List.fold_left (fun map_list segment ->
        match segment with
        | Config(x) ->
          let json = Yojson.Basic.from_string x in
          let inline = Yojson.Basic.Util.member "mappings" json in (
            match inline with
            | `Null -> map_list
            | `Assoc a -> (resolve  a)::map_list
            | _ -> raise Invalid_config_mappings
          )
        | _ -> map_list
      ) [] configs
  in
  inline_mappings@imports


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


