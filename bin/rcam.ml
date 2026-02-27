open Rcamimpl
open Rcamimpl.Rcam

let exec (filename: string) () =
  let inx = Core.In_channel.create filename in
  let input = Core.In_channel.input_all inx in
  let res = Parser.run_parser Parser.parse_program input in
  match res with
    | Error err -> Printf.printf "%s\n" err
    | Ok tree -> 
        Printf.printf "parsed term:\n       %s\n\n" (Syntax_tree.print_syntax_tree tree) ;
        let term = scope_check tree in
        evaluate term

let () =
  Core.Command.basic_spec ~summary:"Parse and display λ-terms"
    Core.Command.Spec.(empty +> anon ("filename" %: string))
    exec
  |> Command_unix.run
