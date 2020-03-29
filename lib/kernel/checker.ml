open Term
open Option
open Unification

type type_env = (string * ptype) list

type error =
  | Bad_type of ptype
  | Not_typable
  | Overwrite
  | Unification

let new_env = [
  "case", (ge "A" @@ ge "P") @@ ((ge "B" @@ ge "P") @@ ((T_or (ge "A", ge "B")) @@ ge "P"));
  "or_l", (ge "A" @@ (T_or (ge "A", ge "B")));
  "or_l", (ge "B" @@ (T_or (ge "A", ge "B")));

  "fst", ((T_and (ge "A", ge "B")) @@ ge "A");
  "snd", ((T_and (ge "A", ge "B")) @@ ge "B");
  "and", (ge "A" @@ (ge "B" @@ (T_and (ge "A", ge "B"))));
]

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
  let rec resolve_application_by_unification app t1 a b tt2 =
    match unify [(a, tt2)] with
    | Error _ -> None
    | Ok u ->
      if u tt2 <> tt2 then (
        Printf.printf "(type checker) (e)  the term %s is too specific !\n" (pretty_term_inline t1);
        None
      ) else (
        Printf.printf "(type checker) (u)  %s : %s \n\n" (pretty_term_inline app) (pretty_type (u b));
        Some (u b)
      )
  and rec_type_check t env =
    match t with
    | Lam (Bind (x, tx), y) ->
      let env' = List.remove_assoc x env in
      let* ty = rec_type_check y ((x, tx) :: env') in
      let tt = T_arrow (tx, ty) in
      Printf.printf "(type checker)      %s : %s\n" (pretty_term_inline t) (pretty_type tt);
      Some tt
    | Var x -> 
      let* t = List.assoc_opt x env in
      Printf.printf "(type checker)      %s : %s\n" x (pretty_type t);
      Some t
    | App (t1, t2) ->
      let* tt1 = rec_type_check t1 env in
      let* tt2 = rec_type_check t2 env in
      match tt1 with
      | T_arrow (a, b) when a = tt2 ->
        Printf.printf "(type checker)      %s : %s\n\n" (pretty_term_inline t) (pretty_type b);
        Some (b)
      | T_arrow (a, b) -> resolve_application_by_unification t t1 a b tt2
      | _ -> None

  in
  rec_type_check t (rename_type_variables env)

let extend_env name target proof env =
  let extend_by_unification t target =
    let t' = (rename_type 1 t) in
    match unify_one t' target with
    | Ok u ->
      if u target <> target then (
        Printf.printf "(type checker) (e)  the proof is too specific, please generalize !\n";
        Error (Bad_type t)
      ) else (
        Printf.printf "(type checker) (w)  the proof is a generalization. Applying it with : ";
        Printf.printf "%s\n" 
          (VarSet.fold (fun x y -> y ^ "'" ^ x ^ " = " ^ (pretty_type (T_gen x |> rename_type 1 |> u)) ^ "; ") (fv t) "");
        Printf.printf "(type checker) (u)  %s : %s\n" (pretty_term_inline proof) (pretty_type (u t'));
        Ok ((name, target)::env)
      )
    | Error Cycling ->
      Printf.printf "(type checker) (e)  Unification failed with 'CYCLING'\n";
      Error (Bad_type t)
    | Error Clash ->
      Printf.printf "(type checker) (e)  Unification failed with 'CLASH'\n";
      Error (Bad_type t)

  in
  match List.assoc_opt name env with
  | Some _ -> Error (Overwrite)
  | None ->
    match type_check proof env with
    | Some t when t = target -> Ok ((name, target)::env)
    | Some t -> extend_by_unification t target
    | _ -> Error (Not_typable)

let extend_by_axiom name axiom env =
  match List.assoc_opt name env with
  | None -> Ok ((name, axiom)::env)
  | _ -> Error (Overwrite)