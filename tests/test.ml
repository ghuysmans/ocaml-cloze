(* examples taken from https://docs.ankiweb.net/editing.html#cloze-deletion *)

open Cloze
open Ast
open Utils

let one = Cat [
  Str "Canberra was founded in ";
  Mask ("c1", Str "1913", None);
  Str "."
]

let two = Cat [
  Mask ("c2", Str "Canberra", None);
  Str " was founded in ";
  Mask ("c1", Str "1913", None);
  Str "."
]

let hint = Cat [
  Mask ("c1", Str "Canberra", Some "city");
  Str " was founded in 1913.";
]

let nested = Cat [
  Mask ("c1", Cat [Str "Canberra was "; Mask ("c2", Str "founded", None)], None);
  Str " in 1913.";
]

let rep x n =
  let rec f acc = function
    | 0 -> acc
    | n -> f (Cat [x; acc]) (n - 1)
  in
  f (Str "") n

let () =
  let f t =
    Printf.printf "%S\n" (to_string t);
    delete None t :: delete_each t |> List.iter (fun (s, l) ->
      Printf.printf "- %S: %s\n" s (String.concat ", " l)
    )
  in
  (* FIXME check the output values! *)
  f one;
  f two;
  f hint;
  f nested;
  ignore (solution (rep (Str "x") 50000))
