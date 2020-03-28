open Term

let rec occurs tvar tterm =
  match tterm with
  | T_atom _ -> false
  | T_gen x -> x = tvar
  | T_arrow (ta, tb)
  | T_or (ta, tb)
  | T_and (ta, tb) -> occurs tvar ta || occurs tvar tb

let rec subst (v, s) t =
  match t with
  | T_gen i -> if i = v then s else t
  | T_arrow (t1, t2) -> T_arrow (subst (v, s) t1, subst (v, s) t2)
  | T_and (t1, t2) -> T_and (subst (v, s) t1, subst (v, s) t2)
  | T_or (t1, t2) -> T_or (subst (v, s) t1, subst (v, s) t2)
  | T_atom _ -> t

let (%) (f : ptype -> ptype) (g : ptype -> ptype) = fun x -> f (g x)

let apply mgu = List.map (fun (x, y) -> (mgu x, mgu y))

type error = Cycling | Clash

let (let*) x f =
  match x with
  | Ok y -> f y
  | Error _ -> x

let rec unify_one a b =
  match a, b with
  | T_gen x, T_gen y -> if x = y then Ok (Fun.id) else Ok (subst (x, b))
  | T_atom a, T_atom b -> if a = b then Ok (Fun.id) else Error Clash
  | T_arrow (a, b), T_arrow (c, d)
  | T_and (a, b), T_and (c, d)
  | T_or (a, b), T_or (c, d) -> unify [(a, c); (b, d)]
  | a, T_gen i
  | T_gen i, a ->
    if occurs i a
    then Error (Cycling)
    else Ok (subst (i, a))
  | _ -> Error Clash

and unify consl =
  match consl with
  | [] -> Ok (Fun.id)
  | (x, y)::rest ->
    let* mgu1 = unify_one x y in
    let* mgu2 = (unify (apply mgu1 rest))  in
    Ok (mgu2 % mgu1)

module VarSet = Set.Make(String)

let rec fv t =
  match t with
  | T_arrow (a, b)
  | T_and (a, b)
  | T_or (a, b) -> VarSet.union (fv a) (fv b)
  | T_gen x -> VarSet.add x (VarSet.empty)
  | _ -> VarSet.empty

let pretty_unifier u t1 t2 =
  let f = VarSet.union (fv t1) (fv t2) in
  let l = VarSet.map (fun x -> Printf.sprintf " ('%s <- %s) " x (pretty_type (u (T_gen x)))) f in
  VarSet.fold (^) l ""