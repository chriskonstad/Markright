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


(* Collect all mappings into a list of mappings *)
exception Invalid_config_mappings
let collectMappings segments =
  List.fold_left (fun map_list segment ->
      match segment with
      | Config(x) ->
        (* TODO Load config files first, then prepend the inlined mappings *)
        let json = Yojson.Basic.from_string x in
        let inline = Yojson.Basic.Util.member "mappings" json in (
          match inline with
          | `Null -> map_list
          | `Assoc a -> (resolve  a)::map_list
          | _ -> raise Invalid_config_mappings
        )
      | _ -> map_list
    ) [] segments


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
exception Missing_mapping
let replace mapping segments =
  try
    List.map (fun n ->
        match n with
        | Text(x) -> Text(x)
        | Variable(x) -> Text(mapping x)
        | Config(x) -> Text("")
      ) segments
  with
  | _ -> raise Missing_mapping


