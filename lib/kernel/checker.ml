open Term
open Option

type type_env = (string * type_info) list
let (let*) = Option.bind

let rec type_check t env =
  match t with
  | Lambda (Bind (_, Type_atom "?"), _) -> None
  | Lambda (Bind (x, tx), y) ->
    let env' = List.remove_assoc x env in
    let* ty = type_check y ((x,tx)::env') in
    Some (Type_arrow (tx, ty))
  | Var x ->
    List.assoc_opt x env
  | Application (t1, t2) ->
    let* tt1 = type_check t1 env in
    let* tt2 = type_check t2 env in
    match tt1, tt2 with
    | Type_arrow (a, b), c when a = c -> Some(b)
    | _ -> None
