open Markright
open Core.Std


let command =
  Command.basic
    ~summary:"Convert a Markright file to Markdown"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
                  +> anon ("config_file" %: file)
                  +> anon ("markright_file" %: file)
                 )
    (fun config text () -> process_file config text |> print_string)

let () =
  Command.run ~version:"0.1" command
