(* TODO Test how trimming affects parsing *)
(* TODO Add support for inline config *)
(* TODO Add support for importing config *)
(* TODO Add support for multiple configs *)

open Core.Std
open Batteries
open Mapper
open Parser

(* With a given json assoc array and id, return the json's value for id *)
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