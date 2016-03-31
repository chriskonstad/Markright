(** Maps variables to actual values *)
open Batteries
open Parser
open Yojson


(** Keep the json and the file it's pulled from together *)
type named_json =
  { json : (string * Yojson.Basic.json) list list;
    file : string;
  }

(** Keep a mapping its source file together *)
type named_mapping =
  { mapping : string;
    file : string;
  }


(** Mapper.stringify reached a data type that it cannot handle *)
exception Bad_stringify
(** Flatten an assoc list of JSON to an assoc list of strings *)
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


(** A variable has multiple definitions *)
exception Multiple_def of string
(** Merge multiple configurations into one, while
    detecting if one config overrides another *)
let rec merge ignore_mult_def configs =
  let merger config base =
    let make_mult_def_msg first second =
      first.mapping ^ " (" ^ first.file ^ ") defined again in " ^ second.file
    in
    List.iter (fun (k,v) ->
        if List.mem_assoc k base then (
          if not ignore_mult_def then
            raise (Multiple_def (make_mult_def_msg (List.assoc k base) v))
        )
      ) config;
    config@base
  in
  List.fold_left (fun ret cur ->
      merger cur ret
    ) [] configs


(** There is a bad mapping in the config *)
exception Invalid_config_mappings
(** There is a bad import in the config *)
exception Invalid_config_imports
(** Recursively load the given configuration *)
let rec loadConfiguration (config : string) (file : string) : named_json list =
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
            let f_name = Filename.concat (Filename.dirname file) f in
            let file = File.open_in f_name in
            let config_text = IO.read_all file in
            (loadConfiguration config_text (f_name))@ret
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
  { json = inline_mappings;
    file = file;
  }::[]@imports


(** Collect all mappings into a single mapping *)
exception Not_config
let collectMappings ?(ignore_mult_def=false) segments file =
  let configs = segments |> List.filter (fun seg ->
      match seg with
      | Config(_) -> true
      | _ -> false
    )
  in
  let collected = List.fold_left (fun maplist c ->
      match c with
      | Config(x) -> (loadConfiguration x file)@maplist
      | _ -> raise Not_config
    ) [] configs in
  let condensed = List.map (fun nj ->
        let configs = nj.json in
        let file = nj.file in
        let strings = List.map (fun config ->
            stringify config []
          ) configs in
        List.map (fun s ->
          List.map (fun (k,v) ->
              (k,{ mapping = v;
                   file = file;
                }) ) s
          ) strings
        |> merge ignore_mult_def
    ) collected in
  merge ignore_mult_def condensed |> List.map (fun (k,v) ->
      (k,v.mapping)
    )


(** Apply the mapping map to the given id *)
let applyMap map id =
  List.assoc id map


(** Replace the variables in the segment list using mapping *)
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
