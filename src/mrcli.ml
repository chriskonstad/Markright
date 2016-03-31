open Markright
open Core.Std

let command =
  Command.basic
    ~summary:"Convert a Markright file to Markdown"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
                  +> flag "-i" no_arg ~doc:" ignore multiple definition errors"
                  +> flag "-w" no_arg ~doc:" treat multiple definition errors as warnings"
                  +> anon ("markright_file" %: file)
                 )
    (fun ignore warn text () ->
       let abs_filename = Filename.concat (Sys.getcwd ()) text in
       let mult_def_handle = match ignore, warn with
         | _, true -> Mapper.Warn
         | true, false -> Mapper.Ignore
         | false, false -> Mapper.Throw
       in
       process_file mult_def_handle abs_filename |> print_string)

let () =
  Command.run ~version:"0.1" command
