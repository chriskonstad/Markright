(* TODO Test how trimming affects parsing *)
(* TODO Change config file format to match inline format *)

open Core.Std
open Batteries
open Mapper
open Parser

(* With a given json assoc array and id, return the json's value for id *)
let markright (text : string) : string =
  let segments = parse text |> filter_empty_text in
  let mappings = (collectMappings segments) in
  replace (applyMap mappings) segments |> flatten


let load_file (f : string) : string =
  let file = File.open_in f in
  IO.read_all file


let process_file (file : string) : string =
  let text = load_file file in
  markright text
