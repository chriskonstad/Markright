(** Interface for the Markright library *)
(* TODO Test how trimming affects parsing *)

open Core.Std
open Batteries
open Mapper
open Parser

(** Run Markright on the given text with the given properties *)
let markright ignore_mult_def (text : string) (file : string) : string =
  let segments = parse text |> filter_empty_text in
  let mapping = (collectMappings segments file ~ignore_mult_def:ignore_mult_def) in
  replace (applyMap mapping) segments |> flatten


(** Load a file up as text *)
let load_file (f : string) : string =
  let file = File.open_in f in
  IO.read_all file


(** Process the given file as a Markright file *)
let process_file ignore_mult_def (file : string) : string =
  let text = load_file file in
  markright ignore_mult_def text file
