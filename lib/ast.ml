type _ t =
  | Str : string -> _ t
  | Cat : 'm t list -> 'm t
  | Mask : string * _ t * string option -> [`With_mask] t

let cat = function
  | [t] -> t
  | l -> Cat l
