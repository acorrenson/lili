open Term
open Option
open Unification

type type_env = (string * ptype) list

type error =
  | Bad_type of ptype
  | Not_typable
  | Overwrite
  | Unification

let new_env = []

let ( let* ) = Option.bind

let rec type_check t env =
  match t with
  | Lam (Bind (x, tx), y) ->
    let env' = List.remove_assoc x env in
    let* ty = type_check y ((x, tx) :: env') in
    Some (T_arrow (tx, ty))
  | Var x -> List.assoc_opt x env
  | App (t1, t2) ->
    let* tt1 = type_check t1 env in
    let* tt2 = type_check t2 env in
    match tt1 with
    | T_arrow (a, b) ->
      (match unify [(a, tt2)] with
       | Error _ -> None
       | Ok u -> Printf.printf "applying unification : %s\n" (pretty_unifier u b);
         Some (u b))
    | _ -> None

let extend_env name target proof env =
  match List.assoc_opt name env with
  | Some _ -> Error (Overwrite)
  | None ->
    match type_check proof env with
    | Some t ->
      (match unify [(t, target)] with
       | Ok u -> Printf.printf "applying unification : %s\n" (pretty_unifier u t);Ok ((name, target)::env)
       | Error _ -> Error (Unification)
      )
    | _ -> Error (Not_typable)

let extend_by_axiom name axiom env =
  match List.assoc_opt name env with
  | None -> Ok ((name, axiom)::env)
  | _ -> Error (Overwrite)