type t =
  | Str of string
  | Cat of t list
  | Mask of string * t list * string option

let rec solution = function
  | Str s -> s
  | Cat l -> String.concat "" (List.map solution l)
  | Mask (_, l, _) -> solution (Cat l) (* TODO unfold? *)

let rec to_string = function
  | Str s -> s
  | Cat l -> String.concat "" (List.map to_string l)
  | Mask (m, l, h) ->
    "{{" ^ m ^ "::" ^
    to_string (Cat l) ^
    (Option.value ~default:"" (Option.map ((^) "::") h)) ^
    "}}"

let rec delete mask = function
  | Str s -> s, []
  | Cat l ->
    let s, a = List.split (List.map (delete mask) l) in
    String.concat "" s, List.concat a
  | Mask (m, l, h) when mask = m ->
    "[" ^ Option.value ~default:"..." h ^ "]",
    [solution (Cat l)] (* FIXME test with nested masks *)
  | Mask (_, l, _) -> delete mask (Cat l)

let rec masks = function
  | Str _ -> []
  | Cat l -> List.concat_map masks l
  | Mask (m, l, _) -> m :: masks (Cat l)

let delete_all t =
  masks t |>
  List.sort_uniq compare |>
  List.map (fun m -> delete m t)


let one = Cat [
  Str "Canberra was founded in ";
  Mask ("c1", [Str "1913"], None);
  Str "."
]

let two = Cat [
  Mask ("c2", [Str "Canberra"], None);
  Str " was founded in ";
  Mask ("c1", [Str "1913"], None);
  Str "."
]

let hint = Cat [
  Mask ("c1", [Str "Canberra"], Some "city");
  Str " was founded in 1913.";
]

let nested = Cat [
  Mask ("c1", [Str "Canberra was "; Mask ("c2", [Str "founded"], None)], None);
  Str " in 1913.";
]

let () =
  let f t =
    Printf.printf "%S\n" (to_string t);
    delete_all t |> List.iter (fun (s, l) ->
      Printf.printf "- %S: %s\n" s (String.concat ", " l)
    )
  in
  f one;
  f two;
  f hint;
  f nested;
