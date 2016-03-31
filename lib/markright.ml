(** Interface for the Markright library *)
(* TODO Test how trimming affects parsing *)

open Core.Std
open Batteries
open Mapper
open Parser

(** Run Markright on the given text with the given properties *)
let markright md_handle (text : string) (file : string) : string =
  let segments = parse text |> filter_empty_text in
  let mapping = (collectMappings segments file ~mult_def_handle:md_handle) in
  replace (applyMap mapping) segments |> flatten


(** Load a file up as text *)
let load_file (f : string) : string =
  let file = File.open_in f in
  IO.read_all file


(** Process the given file as a Markright file *)
let process_file md_handle (file : string) : string =
  let text = load_file file in
  markright md_handle text file
