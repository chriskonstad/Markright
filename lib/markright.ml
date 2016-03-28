(* TODO Test how trimming affects parsing *)

open Core.Std
open Batteries
open Mapper
open Parser

(* With a given json assoc array and id, return the json's value for id *)
let markright (text : string) (dir : string) : string =
  let segments = parse text |> filter_empty_text in
  let mappings = (collectMappings segments dir) in
  replace (applyMap mappings) segments |> flatten


let load_file (f : string) : string =
  let file = File.open_in f in
  IO.read_all file


let process_file (file : string) : string =
  let text = load_file file in
  markright text (Filename.dirname file)
