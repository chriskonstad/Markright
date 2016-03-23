open Batteries
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
