(* TODO Test how trimming affects parsing *)

open Core.Std
open Batteries
open Mapper
open Parser

(* With a given json assoc array and id, return the json's value for id *)
let markright ignore_mult_def (text : string) (dir : string) : string =
  let segments = parse text |> filter_empty_text in
  let mapping = (collectMappings segments dir ~ignore_mult_def:ignore_mult_def) in
  replace (applyMap mapping) segments |> flatten


let load_file (f : string) : string =
  let file = File.open_in f in
  IO.read_all file


let process_file ignore_mult_def (file : string) : string =
  let text = load_file file in
  markright ignore_mult_def text (Filename.dirname file)
