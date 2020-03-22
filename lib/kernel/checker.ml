open Term
open Option

type type_env = (string * ptype) list

type error =
  | Bad_type of ptype
  | Not_typable
  | Overwrite

let new_env = []

let ( let* ) = Option.bind

let rec type_check t env =
  match t with
  | Lam (Bind (_, T_gen _), _) -> None
  | Lam (Bind (x, tx), y) ->
    let env' = List.remove_assoc x env in
    let* ty = type_check y ((x, tx) :: env') in
    Some (T_arrow (tx, ty))
  | Var x -> List.assoc_opt x env
  | App (t1, t2) -> (
      let* tt1 = type_check t1 env in
      let* tt2 = type_check t2 env in
      match (tt1, tt2) with T_arrow (a, b), c when a = c -> Some b | _ -> None )

let extend_env name target proof env =
  match List.assoc_opt name env with
  | Some _ -> Error (Overwrite)
  | None ->
    match type_check proof env with
    | Some t when t = target -> Ok ((name, target)::env)
    | Some t -> Error (Bad_type t)
    | _ -> Error (Not_typable)

let extend_by_axiom name axiom env =
  match List.assoc_opt name env with
  | None -> Ok ((name, axiom)::env)
  | _ -> Error (Overwrite)