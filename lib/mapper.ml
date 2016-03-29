open Batteries
open Parser
open Yojson


(* Flatten an assoc list of JSON to an assoc list of strings *)
exception Bad_stringify
let rec stringify (config : (string * Yojson.Basic.json) list)
    (parents : string list) : (string * string) list =
  List.fold_left (fun c_list pair ->
      match pair with
      | k, value ->
        (
          match value with
          | `String v ->
            ((String.concat "." (parents@[k])), v)::c_list
          | `Assoc v ->
            (stringify v (parents@[k]))@c_list
          | _ -> raise Bad_stringify
        )
    ) [] config


(** Merge multiple configurations into one, while
    detecting if one config overrides another *)
exception Multiple_def of string
let rec merge ignore_mult_def configs =
  let merger config base =
    List.iter (fun (k,_) ->
        if List.mem_assoc k base then (
          if not ignore_mult_def then
            raise (Multiple_def k)
        )
      ) config;
    config@base
  in
  List.fold_left (fun ret cur ->
      merger cur ret
    ) [] configs


(* Load a configuration given as text *)
exception Invalid_config_mappings
exception Invalid_config_imports
let rec loadConfiguration (config : string) (dir : string) =
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
    | `Assoc a -> a::[]
    | _ -> raise Invalid_config_mappings
  in
  inline_mappings@imports


(* Collect all mappings into a list of mappings *)
exception Not_config
let collectMappings ?(ignore_mult_def=false) segments dir =
  let configs = segments |> List.filter (fun seg ->
      match seg with
      | Config(_) -> true
      | _ -> false
    )
  in
  let collected = List.fold_left (fun maplist c ->
      match c with
      | Config(x) -> (loadConfiguration x dir)@maplist
      | _ -> raise Not_config
    ) [] configs in
  List.map (fun config ->
      stringify config []
    ) collected |> merge ignore_mult_def


let applyMap map id =
  List.assoc id map


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
