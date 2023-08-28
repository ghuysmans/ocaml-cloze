type t =
  | Str of string
  | Cat of t list
  | Mask of string * t list * string option

let rec solution = function
  | Str _ as t -> t
  | Cat l | Mask (_, l, _) -> Cat (List.map solution l)

let rec to_string = function
  | Str _ as t -> t
  | Cat l -> Cat (List.map to_string l)
  | Mask (m, l, h) -> Cat [
    Str "{{"; Str m; Str "::";
    to_string (Cat l);
    Str (Option.value ~default:"" (Option.map ((^) "::") h));
    Str "}}"
  ]

let rec delete mask = function
  | Str _ as t -> t, []
  | Cat l ->
    let t, a = List.split (List.map (delete mask) l) in
    Cat t, List.concat a
  | Mask (m, l, h) when mask = None || mask = Some m ->
    Cat [Str "["; Str (Option.value ~default:"..." h); Str "]"],
    [solution (Cat l)] (* FIXME test with nested masks *)
  | Mask (_, l, _) -> delete mask (Cat l)

let concat t =
  let rec f acc = function
    | Mask _ -> failwith "FIXME eliminate this case using a GADT"
    | Str s -> s :: acc
    | Cat l -> List.fold_right (fun x acc -> f acc x) l acc
  in
  String.concat "" (f [] t)

let solution t = concat (solution t)
let to_string t = concat (to_string t)
let delete mask t =
  let t, a = delete mask t in
  concat t, List.map concat a

let rec masks = function
  | Str _ -> []
  | Cat l -> List.concat_map masks l
  | Mask (m, l, _) -> m :: masks (Cat l)

let delete_each t =
  masks t |>
  List.sort_uniq compare |>
  List.map (fun m -> delete (Some m) t)


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
    delete None t :: delete_each t |> List.iter (fun (s, l) ->
      Printf.printf "- %S: %s\n" s (String.concat ", " l)
    )
  in
  f one;
  f two;
  f hint;
  f nested;
