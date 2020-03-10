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
  Printf.sprintf "%s:%s" x (pretty_type t)

let rec pretty_term t =
  match t with
  | Lambda (b, t') ->
    Printf.sprintf "(ƛ%s.%s)"
      (pretty_binding b) (pretty_term t')
  | Application (a, b) ->
    Printf.sprintf "(%s %s)"
      (pretty_term a) (pretty_term b)
  | Var x -> x
