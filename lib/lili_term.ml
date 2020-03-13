type type_info =
  | Type_atom of string
  | Type_arrow of type_info * type_info
[@@deriving variants]

type binding = Bind of string * type_info
[@@deriving variants]

type term =
  | Lambda of binding * term
  | Application of term * term
  | Var of string
[@@deriving variants]

let rec pretty_type t =
  match t with
  | Type_atom s -> s
  | Type_arrow (Type_atom a, b) ->
    Printf.sprintf "%s -> %s" a (pretty_type b)
  | Type_arrow (a, b) ->
    Printf.sprintf "(%s) -> %s" (pretty_type a) (pretty_type b)

let pretty_binding (Bind (x, t)) =
  Printf.sprintf "%s:(%s)" x (pretty_type t)

let pretty_term t =
  let rec rec_pretty_term t p n =
    match t with
    | Lambda (b, t') ->
      Printf.sprintf "%sλ%s.\n%s%s"
        (String.make p ' ') (pretty_binding b) (String.make (n+2+p) ' ') (rec_pretty_term t' 0 (n+2+p))
    | Application (Var s, b) ->
      Printf.sprintf "(%s %s)"
        s (rec_pretty_term b 0 (n+2))
    | Application (a, b) ->
      Printf.sprintf "(%s\n%s)"
        (rec_pretty_term a 0 (n+2)) (rec_pretty_term b 1 (n+2))
    | Var x -> x
  in
  rec_pretty_term t 0 0