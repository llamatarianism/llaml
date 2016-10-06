open Parser
open Lexer
open Ast
       
       
let () =
  let filename = Sys.argv.(1) in
  let chan = filename |> open_in |> Lexing.from_channel in
  match Parser.main Lexer.read chan with
  | [] -> print_endline "Jack shit."
  | xs -> List.iter (fun x -> print_endline (show_statement x)) xs
                    
