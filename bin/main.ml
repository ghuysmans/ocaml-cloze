open Cloze
open Utils

let () =
  let t = Parser.main Lexer.tokenize (Lexing.from_channel stdin) in
  Printf.printf "%S\n" (to_string t);
  delete None t :: delete_each t |> List.iter (fun (s, l) ->
    Printf.printf "- %S: %s\n" s (String.concat ", " l)
  )
