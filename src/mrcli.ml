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
    (fun ignore text () ->
       let abs_filename = Filename.concat (Sys.getcwd ()) text in
       process_file ignore abs_filename |> print_string)

let () =
  Command.run ~version:"0.1" command
