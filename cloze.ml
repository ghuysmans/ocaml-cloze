type t =
  | Str of string
  | Cat of t list
  | Mask of string * t list * string option

let rec to_string ~fmt = function
  | Str s -> s
  | Cat l -> String.concat "" (List.map (to_string ~fmt) l)
  | Mask (m, l, h) ->
    match fmt with
    | `Delete mask when mask = m ->
      "[" ^ Option.value ~default:"..." h ^ "]"
    | `Delete _ | `Solution ->
      to_string ~fmt (Cat l) (* TODO unfold? *)
    | `Raw ->
      "{{" ^ m ^ "::" ^
      to_string ~fmt (Cat l) ^
      (Option.value ~default:"" (Option.map ((^) "::") h)) ^
      "}}"

let rec masks = function
  | Str _ -> []
  | Cat l -> List.concat_map masks l
  | Mask (m, l, _) -> m :: masks (Cat l)

let delete_all t =
  masks t |>
  List.sort_uniq compare |>
  List.map (fun m -> to_string ~fmt:(`Delete m) t)


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

let () =
  let f t =
    Printf.printf "%S: %s\n"
      (to_string ~fmt:`Raw t)
      (String.concat ", " (masks t))
  in
  f one;
  f two
