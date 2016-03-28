open Markright
open Core.Std


let command =
  Command.basic
    ~summary:"Convert a Markright file to Markdown"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
                  +> anon ("markright_file" %: file)
                 )
    (fun text () -> process_file text |> print_string)

let () =
  Command.run ~version:"0.1" command
