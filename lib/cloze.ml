type _ t =
  | Str : string -> _ t
  | Cat : 'm t list -> 'm t
  | Mask : string * _ t list * string option -> [`With_mask] t

let rec solution : type m. m t -> _ t = function
  | Str _ as t -> t
  | Cat l -> Cat (List.map solution l)
  | Mask (_, l, _) -> Cat (List.map solution l)

let rec to_string : type m. m t -> _ t = function
  | Str _ as t -> t
  | Cat l -> Cat (List.map to_string l)
  | Mask (m, l, h) -> Cat [
    Str "{{"; Str m; Str "::";
    to_string (Cat l);
    Str (Option.value ~default:"" (Option.map ((^) "::") h));
    Str "}}"
  ]

let rec delete : type m. string option -> m t -> _ t * _ t list =
fun mask -> function
  | Str _ as t -> t, []
  | Cat l ->
    let t, a = List.split (List.map (delete mask) l) in
    Cat t, List.concat a
  | Mask (m, l, h) when mask = None || mask = Some m ->
    Cat [Str "["; Str (Option.value ~default:"..." h); Str "]"],
    [solution (Cat l)] (* FIXME test with nested masks *)
  | Mask (_, l, _) -> delete mask (Cat l)

let concat t =
  let rec f : string list -> [`Without_mask] t -> string list =
  fun acc -> function
    | Str s -> s :: acc
    | Cat l -> List.fold_right (fun x acc -> f acc x) l acc
  in
  String.concat "" (f [] t)

let solution t = concat (solution t)
let to_string t = concat (to_string t)
let delete mask t =
  let t, a = delete mask t in
  concat t, List.map concat a

let rec masks : type m. m t -> string list = function
  | Str _ -> []
  | Cat l -> List.concat_map masks l
  | Mask (m, l, _) -> m :: masks (Cat l)

let delete_each t =
  masks t |>
  List.sort_uniq compare |>
  List.map (fun m -> delete (Some m) t)
