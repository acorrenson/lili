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

let rec rename_type i t =
  match t with
  | T_gen a -> T_gen (a ^ (string_of_int i))
  | T_arrow (a, b) ->
    T_arrow (rename_type i a, rename_type i b)
  | T_and (a, b) ->
    T_and (rename_type i a, rename_type i b)
  | T_or (a, b) ->
    T_or (rename_type i a, rename_type i b)
  | T_atom _ -> t


let rename_type_variables env =
  List.mapi (fun i (id, t) -> (id, rename_type i t)) env


let type_check t env =
  let rec resolve_application_by_unification app t1 a b t2 tt2 =
    match unify [(a, tt2)] with
    | Error _ -> None
    | Ok u ->
      let tt1 = T_arrow (a, b) in
      Printf.printf "(type checker) applying unifier %s\n" (pretty_unifier u a b);
      Printf.printf "(type checker) with %s : %s \n" (pretty_term_inline t1) (pretty_type (u tt1));
      Printf.printf "(type checker) with %s : %s \n" (pretty_term_inline t2) (pretty_type (u tt2));
      Printf.printf "(type checker) |- %s : %s \n\n" (pretty_term_inline app) (pretty_type (u b));
      Some (u b)
  and rec_type_check t env =
    match t with
    | Lam (Bind (x, tx), y) ->
      let env' = List.remove_assoc x env in
      let* ty = rec_type_check y ((x, tx) :: env') in
      Some (T_arrow (tx, ty))
    | Var x -> List.assoc_opt x env
    | App (t1, t2) ->
      let* tt1 = rec_type_check t1 env in
      let* tt2 = rec_type_check t2 env in
      Printf.printf "(type checker) |- %s : %s\n" (pretty_term_inline t1) (pretty_type tt1);
      Printf.printf "(type checker) |- %s : %s\n" (pretty_term_inline t2) (pretty_type tt2);
      match tt1 with
      | T_arrow (a, b) when a = tt2 ->
        Printf.printf "(type checker) |- %s : %s\n" (pretty_term_inline t) (pretty_type b);
        Some (b)
      | T_arrow (a, b) -> resolve_application_by_unification t t1 a b t2 tt2
      | _ -> None

  in
  rec_type_check t (rename_type_variables env)

let extend_env name target proof env =
  let extend_by_unification t target =
    match unify_one t target with
    | Ok u ->
      Printf.printf "(type checker) |- %s : %s\n" (pretty_term_inline proof) (pretty_type t);
      Printf.printf "(type checker) applying unifier : %s\n" (pretty_unifier u t target);
      Printf.printf "(type checker) |- %s : %s\n" (pretty_term_inline proof) (pretty_type (u t));
      Ok ((name, target)::env)
    | Error Cycling ->
      Printf.printf "(type checker) Unification failed with 'CYCLING'\n";
      Error (Unification)
    | Error Clash ->
      Printf.printf "(type checker) Unification failed with 'CLASH'\n";
      Error (Unification)
  in
  match List.assoc_opt name env with
  | Some _ -> Error (Overwrite)
  | None ->
    match type_check proof env with
    | Some t when t = target -> Ok ((name, target)::env)
    | Some t -> extend_by_unification (rename_type 1 t) target
    | _ -> Error (Not_typable)

let extend_by_axiom name axiom env =
  match List.assoc_opt name env with
  | None -> Ok ((name, axiom)::env)
  | _ -> Error (Overwrite)