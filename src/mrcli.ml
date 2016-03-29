open Markright
open Core.Std


let command =
  Command.basic
    ~summary:"Convert a Markright file to Markdown"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
                  +> flag "-i" no_arg ~doc:" ignore multiple definition errors"
                  +> anon ("markright_file" %: file)
                 )
    (fun ignore text () -> process_file ignore text |> print_string)

let () =
  Command.run ~version:"0.1" command
